
 # QuickForm: Type level HTML forms

* Concise, modular definitions
* Full and partial validation

 ## Example

This example is a literate haskell file, so let's start with the language
extensions and imports we will be needing.

```haskell

> {-# LANGUAGE DataKinds, InstanceSigs, TypeOperators #-}

> module Example where

> import Data.Maybe (catMaybes, isJust)
> import Data.Text (Text)
> import qualified Data.Set as S
> import qualified Data.Text as T
> import QuickForm

```

In this example, our form should allow a user to sign up with their email
and password, along with their favourite colour and film. We might wish to
obtain some data types like so:

```haskell

> newtype Email = Email Text deriving Show
> newtype Password = Password Text deriving Show
> data Colour = Red | Yellow | Pink | Green | Orange | Purple | Blue
>   deriving (Enum, Read, Show)
> newtype Film = Film Text deriving Show

```

Now let's build our form type.

First, you need to know about `NamedField`. These are the most basic form
elements, and just represent concrete HTML fields. `NamedField` takes two type
parameters, the first of kind `Symbol` (which is a type level string), and the
second of kind `FieldType`. The `Symbol` is used as the field's HTML `name`. The
`FieldType` denotes the element used to display it, governing how values are
marshalled to haskell types: for example, `<input>` elements can be thought of
as storing `Text` values.

Perhaps we want two password fields (so that we can check that they match):

```haskell

> type EnterPasswordField = NamedField "password" TextField
> type RepeatPasswordField = NamedField "password-repeat" TextField

```

We want different type of `FieldType` to construct the colour field. We encode
any enumerable field (dropdown box, radio fields) in `EnumField t` where `t` is
the type we want to use to provide the field data and get back after validation.

```haskell

> type ColourField = NamedField "colour" (EnumField Colour)

```

Now let's look at the sub form combinator `:<:` and unvalidated forms. Our
`Film` field is a good example: we don't care about what the user enters, we
just want to put it into our `Film` newtype. `UnvalidatedForm` captures this
idea: it takes a single parameter of the type we want to get from the sub form.

`a :<: b` denotes that `b` is a sub form of `a`, such that `a` depends on the
values of `b` to perform validation. All forms *must* perform validation in
order for you to get your haskell types, unvalidated forms just convert to it
with no potential for failure.

```haskell

> type FilmField = UnvalidatedForm Film :<: NamedField "film" TextField

```

Similarly we can use `ValidatedForm` for our `Email` type. We'd like to validate
this to check it is a valid email address, so we make a simple sum type
encapsulating some properties we want to check:

```haskell

> data EmailError = InvalidEmail | EmailAlreadyExists deriving (Eq, Ord, Show)

```

`ValidatedForm` takes two type parameters, the first is a type that encodes our
errors, the second is the validated type (similar to `UnvalidatedForm`). You can
think of this as being like `Either`, with the left hand side representing
failure and the right hand side representing success.

```haskell

> type EmailField = ValidatedForm EmailError Email :<: NamedField "email" TextField

```

Now we need to introduce the "pair" combinator `a :&: b`. This encodes two
equal level fields or sub forms. This can be used in a sub form combinator, and
doing so will mean the parent form depends on all values in order to be
validated. You can also chain them together to make forms of arbitrary length,
think of it like a tuple type's comma.

Our password field will need some potential errors, let's keep it simple:

```haskell

> data PasswordError = TooShort | Unmatching deriving (Eq, Ord, Show)

```

We construct the password form from the two base fields that we defined earlier.
Note that the fixity of `:&:` is higher than `:<:`.

```haskell

> type PasswordField = ValidatedForm PasswordError Password
>                  :<: EnterPasswordField :&: RepeatPasswordField

```

Bringing the fields together to make the whole form gives:

```haskell

> type UserForm = EmailField :&: PasswordField :&: ColourField :&: FilmField

```

So far this is quite simple, we just defined the shape of our form at the type
level. But we need a way of operating at the term level in order to actually do
anything. Enter `Form`. `Form` is a newtype, wrapping a type function which
strips superfluous information out of our form definition. It also carries some
metadata (the reduction type, and the original form type).

`Form` takes two type parameters, the first parameter of kind `Reduced` (tells
us how the form has been reduced), and the second of any form.
The `Reduced` kind comprises of types: `Raw`, `Err`, or `Hs`.
* `Raw` is the raw input type, only encoding the actual form values.
* `Err` encodes any potential errors of the form.
* `Hs` encodes the final "haskell" values which we would like.

Loading this file into GHCI within the context of this repository, you can take
a look at what these mean (these examples are slightly reformatted):

```
ghci> :kind! Form 'Raw UserForm
Form 'Raw UserForm :: *
= Form' 'Raw UserForm (Text :&: (Text :&: Text) :&: Text :&: Text)

ghci> :kind! Form 'Err UserForm
Form 'Err UserForm :: *
= Form' 'Err UserForm
   ( Maybe (Set EmailError)
 :&: Maybe (Set PasswordError)
 :&: Maybe (Set EnumError))

ghci> :kind! Form 'Hs UserForm
Form 'Hs UserForm :: *
= Form' 'Hs UserForm (Email :&: Password :&: Colour :&: Film)
```

`:kind!` allows you to evaluate type functions in ghci. You may notice that the
structure of these is roughly like the type we laid out earlier, but with parts
missing when they are not used in that particular representation. For example,
the error type omits the unvalidated film field completely, and the haskell type
drops the inner `NamedField` information.

 # Validation class

As I mentioned earlier, all fields need validating in order to get to our
haskell types. The logic for validation is captured in the conveniently named
type class, `Validation form`. It has one function, `validate`, the type of
which changes depending on what `form` is in the instance head. There are only
two valid uses: on parent forms like `UnvalidatedForm a :<: b` or `ValidatedForm
e a :<: b`. In the first case, `validate :: Form 'Hs b -> a`, and the second,
`validate :: Form 'Hs b -> Validate e a`. `Validate` is isomorphic to `Either`,
with `Invalid` instead of `Left` and `Valid` instead of `Right`.

Let's have a look at our example; first off, the unvalidated film field. The sub
field in this case is `NamedField "film" 'TextField`, so the haskell type will
just be `Text`. Since it is unvalidated, we can always produce a `Film` value
given a `Text` value.

```haskell

> instance Validation FilmField where
>   validate = Film . unForm

```

We just unwrap the text that is in the form, and wrap it in `Film`.

Now for a more interesting example, the email field. This is again just wrapping
a single text field, but it is validated! So the return type changes to
`Validate EmailError Email`. For this example we will just check if there is an
`@` character in the field.

```haskell

> instance Validation EmailField where
>   validate (Form t)
>     | hasAtSymbol = Valid $ Email t
>     | otherwise = Invalid $ S.singleton InvalidEmail
>     where hasAtSymbol = isJust $ T.find (== '@') t

```

Finally we come to the double password field.

```haskell

> instance Validation PasswordField where
>   validate (Form (t :&: t'))
>     | S.null set = Valid $ Password t
>     | otherwise  = Invalid set
>     where set = S.fromList $ catMaybes [tooShort, matching]
>           tooShort = if T.length t >= 8 then Nothing else Just TooShort
>           matching = if t == t' then Nothing else Just Unmatching

```

 # segway to lenses

 # full validation

 # partial validation







