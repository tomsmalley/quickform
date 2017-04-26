
 # QuickForm: Type level HTML forms

* Concise, modular definitions
* Full and partial validation

 ## Example

This example is a literate haskell file, so let's start with the language
extensions and imports we will be needing.

```haskell

> {-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings
>            , TypeApplications, TypeOperators #-}
> {-# OPTIONS_GHC -Wno-missing-signatures #-}

> module Main where

> import Control.Lens
> import Data.Maybe (catMaybes, isJust)
> import Data.Text (Text)
> import qualified Data.Set as S
> import qualified Data.Text as T
> import QuickForm

```

To load this file, clone the repository and run
`stack ghci quickform:test:README`. Code might appear slightly strange on
github, this is due to mixing literate haskell and markdown syntax to the best
of my ability.

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

Now let's build our form type. We have a type level few combinators available to
us, all of which have kind `QuickForm`, made available by DataKinds. This ensures
you can only construct logically valid form structures.

First, you need to know about `Named`. These are the most basic form
elements, and just represent concrete HTML fields. `Named` takes two type
parameters, the first of kind `Symbol` (which is a type level string), and the
second of kind `FieldType`. The `Symbol` is used as the field's HTML `name`. The
`FieldType` denotes the element used to display it, governing how values are
marshalled to haskell types: for example, `<input>` elements can be thought of
as storing `Text` values.

Perhaps we want two password fields (so that we can check that they match):

```haskell

> type EnterPasswordField = Named "password" TextField
> type RepeatPasswordField = Named "password-repeat" TextField

```

We want different type of `FieldType` to construct the colour field. We encode
any enumerable field (dropdown box, radio fields) in `EnumField t` where `t` is
the type we want to use to provide the field data and get back after validation.

```haskell

> type ColourField = Named "colour" (EnumField Colour)

```

Now let's look at the combinator `Unvalidated a f`, which wraps a sub form
`f :: QuickForm` and allows conversion of the sub form to type `a`. Our
`Film` field is a good example: we don't care about what the user enters, we
just want to put it into our `Film` newtype.

All forms *must* perform validation in order for you to get haskell types back,
unvalidated forms just convert to it with no potential for failure.

```haskell

> type FilmField = Unvalidated Film (Named "film" TextField)

```

Similarly we can use `Validated e a f` for our `Email` type. We'd like to validate
this to check it is a valid email address, so we make a simple sum type
encapsulating some properties we want to check:

```haskell

> data EmailError = InvalidEmail | EmailAlreadyExists deriving (Eq, Ord, Show)

```

`Validated e a f` takes three type parameters, the first is a type that encodes
our errors, the second is the validated type (similar to `Unvalidated`), and the
third is the sub form `f :: QuickForm`. You can remember the order of the first two
arguments by thinking of the similarity with `Either`, with the left hand side
representing failure and the right hand side representing success.

```haskell

> type EmailField = Validated EmailError Email (Named "email" TextField)

```

Now we need to introduce the "pair" combinator `a :+: b`. This encodes two
equal level fields or sub forms. This can be used in a sub form combinator, and
doing so will mean the parent form depends on all values in order to be
validated. You can also chain them together to make forms of arbitrary length,
think of it like a general tuple type.

Our password field will need some potential errors, let's keep it simple:

```haskell

> data PasswordError = TooShort | Unmatching deriving (Eq, Ord, Show)

```

We construct the password form from the two base fields that we defined earlier.

```haskell

> type PasswordField = Validated PasswordError Password
>                     (EnterPasswordField :+: RepeatPasswordField)

```

Bringing the fields together to make the whole form gives:

```haskell

> type UserForm = EmailField :+: PasswordField :+: ColourField :+: FilmField

```

So far this is quite simple, we just defined the shape of our form at the type
level. But we need a way of operating at the term level in order to actually do
anything. Enter `Form`. `Form` is a newtype, wrapping a type function which
strips superfluous information out of our form definition, and "unlifting" it
from kind `QuickForm` to kind `Type`, so we can actually use if. It also carries
some metadata (the reduction kind, and the original form kind).

`Form` takes two type parameters, the first parameter of kind `Reduced` (tells
us how the form has been reduced), and the second of kind `QuickForm`.
The `Reduced` kind comprises of types:
* `Raw` encodes the raw input type, only encoding the actual form values.
* `Err` encodes any potential errors of the form.
* `Hs` encodes the final "haskell" values which we would like.

Loading this file into GHCi within the context of this repository, you can take
a look at what these mean (these examples are slightly reformatted):

```
ghci> :kind! Form Raw UserForm
Form Raw UserForm :: *
= Form' Raw UserForm (Text :&: (Text :&: Text) :&: Text :&: Text)

ghci> :kind! Form Err UserForm
Form Err UserForm :: *
= Form' Err UserForm
   ( Maybe (Set EmailError)
 :&: Maybe (Set PasswordError)
 :&: Maybe (Set EnumError))

ghci> :kind! Form Hs UserForm
Form Hs UserForm :: *
= Form' Hs UserForm (Email :&: Password :&: Colour :&: Film)
```

`:kind!` allows you to evaluate type functions in ghci. You may notice that the
structure of these is roughly like the type we laid out earlier, but with parts
missing when they are not used in that particular representation. For example,
the error type omits the unvalidated film field completely, and the haskell type
drops the inner `Named` information.

 ### Validation class

As I mentioned earlier, all fields need validating in order to get to our
haskell types. The logic for validation is captured in the conveniently named
type class, `Validation f`. It has one function, `validate`, the type of
which changes depending on what `form` is in the instance head. There are only
two valid uses: on parent forms like `Unvalidated a b` or `Validated
e a b`. In the first case, `validate :: Form Hs b -> a`, and the second,
`validate :: Form Hs b -> Validate e a`. `Validate e a` is isomorphic to
`Either (Set e) a`, with `Invalid` instead of `Left` and `Valid` instead of
`Right`. We always return a `Set` of errors, because many errors might be
applicable at once.

It is important to note that this type class encapsulates validation as a pure
function, which can be called on *both* the server and the client. Additional
serverside validation is done in the handler, after performing full form
validation.

Let's have a look at our example; first off, the unvalidated film field. The sub
field in this case is `Named "film" TextField`, so the haskell type will
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

Finally we come to the double password field. We want the password to be at
least 8 characters long, and both fields to match. We just pattern match on
`:&:` to extract the `Text`s.

```haskell
instance Validation PasswordField where
  validate (Form (t :&: t'))
    | S.null set = Valid $ Password t
    | otherwise  = Invalid set
    where set = S.fromList $ catMaybes [tooShort, matches]
          tooShort = if T.length t >= 8 then Nothing else Just TooShort
          matches = if t == t' then Nothing else Just Unmatching
```

With that last example it may strike you as being somewhat dangerous, given that
you could easily mix up two of the raw fields since they are the same type. For
a simple example where both fields should be identical, it isn't such an issue,
but for differing fields it could be a problem.
Indeed, we get no more safety than a typical haskell function like `Text -> Text
-> a`, but there is another option: lenses!

 ### Lens into forms

QuickForm exposes a single overloaded lens, `subform`. It allows you to lens
into any part of a form purely by the type. First we will make an example `Raw`
entry for `UserForm`.

```haskell

> dog :: Form Raw UserForm
> dog = Form $ "dave@dog.com"
>           :&: ("woof" :&: "woof2")
>           :&: "Bone"
>           :&: "Beethoven"

```

Now let's view some fields:

```haskell

> rawEmailField = dog ^. subform @EmailField
> rawPasswordField = dog ^. subform @PasswordField
> rawEnterPasswordField = dog ^. subform @EnterPasswordField

```

The `subform` lens needs the type specifying using the `TypeApplications`
language extension. It's similar to if `subform` was defined to take a `Proxy`
argument, but it takes less typing. You can load this into ghci and test it out:

```
ghci> :t rawEmailField
rawEmailField :: Text
ghci> rawEmailField
"dave@dog.com"
ghci> rawPasswordField
"woof" :&: "woof2"
ghci> rawEnterPasswordField
"woof"
```

If we try to access something which isn't a valid form, it won't have the right
kind and we will get a compile error telling us the kinds don't match. But what
if we try to access a valid subform which doesn't actually exist in the form?

```
ghci> dog ^. subform @(Named "not here" TextField)

<interactive>:45:8: error:
    • Attempted access of sub form
        ‘'Named "not here" TextField’
      But it does not exist in the given form
        ‘'Validated EmailError Email (Named "email" TextField)
         ':+: (PasswordField ':+: (ColourField :+: FilmField))’
    • In the second argument of ‘(^.)’, namely
        ‘subform @(Named "not here" TextField)’
      In the expression: dog ^. subform @(Named "not here" TextField)
      In an equation for ‘it’:
          it = dog ^. subform @(Named "not here" TextField)
```

We get a nice custom type error. `subform` also works on haskell and error
forms:

```haskell

> cat :: Form Hs UserForm
> cat = Form $ Email "top@cat.com"
>          :&: Password "meow"
>          :&: Purple
>          :&: Film "The Pink Panther"

```
```
ghci> cat ^. subform @EmailField
Email "top@cat.com"
```

For both haskell and error type forms, fields which are in the raw form can be
erased, so what happens when we try to view them?

```
ghci> cat ^. subform @EnterPasswordField

<interactive>:63:7: error:
    • Attempted access of erased sub form
        ‘'Named "password" TextField’
      It exists in the given form
        ‘'Validated PasswordError Password
         (EnterPasswordField :+: RepeatPasswordField)’
      But not after the form is reduced to its haskell type
        ‘Password’
    • In the second argument of ‘(^.)’, namely
        ‘subform @EnterPasswordField’
      In the expression: cat ^. subform @EnterPasswordField
      In an equation for ‘it’: it = cat ^. subform @EnterPasswordField
```

Another custom type error, telling us that `EnterPasswordField` has been erased.
We can rewrite the `Validation PasswordField` instance now, in this case it
isn't such a win, but this is just an example!

```haskell

> instance Validation PasswordField where
>   validate passwords
>     | S.null s = Valid $ Password enterP
>     | otherwise  = Invalid s
>     where s = S.fromList $ catMaybes [tooShort, matches]
>           tooShort = if T.length enterP >= 8 then Nothing else Just TooShort
>           matches = if enterP == repeatP then Nothing else Just Unmatching
>           enterP = passwords ^. subform @EnterPasswordField
>           repeatP = passwords ^. subform @RepeatPasswordField

```

 ### Full form validation

QuickForm provides a function, `validateAll`, which handles full form
validation, that is, conversion to the haskell type or error type. The return
type depends on if any field is `Validated`. If the form has no fields which can
fail, it has the type `Form Raw f -> Form Hs f` and is just a direct
conversion. In any other case the type is `Form Raw f -> Either (Form Err f)
(Form Hs f)`. Provided you've written all of the required `Validation`
instances, which we have:

```
ghci> validateAll dog
Left (Form (Just (fromList [])
        :&: Just (fromList [TooShort,Unmatching])
        :&: Just (fromList [EnumReadFailed])))
```

Validation failed, as we would expect for the example given. Let's make one that
passes the rules:

```haskell

> monkey :: Form Raw UserForm
> monkey = Form $ "matt@monkey.com"
>             :&: ("ilikebananas" :&: "ilikebananas")
>             :&: "Yellow"
>             :&: "Planet of the Apes"

```
```
ghci> validateAll monkey
Right (Form (Email "matt@monkey.com"
         :&: Password "ilikebananas"
         :&: Yellow
         :&: Film "Planet of the Apes"))
```

This function must always be called on the server after receiving the raw values
from the client, then you can simply pattern match on the result. If you have
some impure validation to do, for example checking if an email already exists in
your database, you should do this in your handler and then return an appropriate
response. TODO: provide full server / client example with servant.

The function should be called by QuickForm's front end library when the user
submits a form, preventing a needless roundtrip to find errors. Only when it
passes should a request be sent.

 ### Partial form validation

A common feature of HTML forms is validation as you type. We could call
`validateAll` at every field update, but that may be slow with larger forms.
Additionally, typing anything into the first field would cause the later fields
to also be validated, as if you'd pressed the submit button. Ideally we want to
be able to validate a single branch of the form, identified by which field was
updated. In comes `validateBranch :: Form Raw f -> Form Err f`. Because
this does not validate the entire form, we always return an error form. This
function can't be called by forms with no fields that can fail.

You might have noticed that in the earlier error form example, each of the error
sets was wrapped in `Maybe`. This seems rather pointless on the surface, since
we can encode the lack of errors with an empty set. In actuality, we can use
`Nothing` to represent "not checked" and `Just mempty` to represent "passed
validation". This is important when it comes to combining error form structures
on the front end, to prevent error messages being wrongly cleared or sticking
around when they should be deleted.

```haskell

> vemail = validateBranch @EmailField dog
> venterp = validateBranch @EnterPasswordField dog
> vcolour = validateBranch @ColourField dog
> vfilm = validateBranch @FilmField dog

```
```
ghci> vemail
Form (Just (fromList []) :&: Nothing :&: Nothing)
ghci> venterp
Form (Nothing :&: Just (fromList [TooShort,Unmatching]) :&: Nothing)
ghci> vcolour
Form (Nothing :&: Nothing :&: Just (fromList [EnumReadFailed]))
ghci> vfilm
Form (Nothing :&: Nothing :&: Nothing)
```

Notice that the `Just` fields are the ones we asked it to validate, and the
unrelated `Nothing` fields are untouched.

 ### Other notes

 #### Field uniqueness

The library assumes that each of your types for your fields are unique. This
*should always* be the case naturally thanks to the `Symbol` in the terminal
fields - each control on a HTML form should have a different name!





---

For the test suite:

```haskell

> main :: IO ()
> main = putStrLn $ "\nREADME.md"

```

