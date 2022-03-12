# Optics

At the most basic level, and the main reason for using them with Monomer, optics
simplify accessing and updating deeply nested data structures in a composable
way.

Using optics the limitations of Haskell's record system can be overcome, plus
they also offer a flexibility seldom found in other languages.

In Monomer they are used for two main purposes:

- Associating widgets with a field in our model.
- Performing changes to our model during event handling.

Both scenarios can be handled without optics but, in general, it's easier to use
them.

You probably have heard about lenses, which are a subtype of the broader concept
of optics. The main optics library is also called `lens`, and it is the one we
will be using.

## Lenses

In the case of lenses, they provide two guarantees:

- The item they focus on exists.
- They focus on a single item.

This makes them particularly useful for using them with widgets, since they
except a single value they can read/write to.

### Creating lenses

You will need to enable a couple of extensions before using lenses. If you miss
them you will get a compilation error.

If you follow the ideas shown in the tutorials, you need at least:

```haskell
{-# LANGUAGE TemplateHaskell #-}
```

Depending on what you do, you may also need a combination of these:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
```

The main module of the library should be imported too:

```haskell
import Control.Lens
```

The next step is defining your data types (notice the underscore prefix):

```haskell
data Address = Address {
  _street :: Text,
  _doorNumber :: Int
} deriving (Eq, Show)

data Person = Person {
  _name :: Text,
  _homeAddress :: Address,
  _workAddress :: Maybe Address,
  _friends :: [Person]
} deriving (Eq, Show)
```

And, finally, lenses can be created:

```haskell
makeLenses ''Address
makeLenses ''Person
```

The example creates a few test values:

```haskell
address1 = Address "Street 1" 1234
address2 = Address "Avenue 3" 987
address3 = Address "Boulevard 9" 4756

person1 = Person "Mark" address1 Nothing []
person2 = Person "Jane" address2 (Just address3) [person1]
person3 = Person "Zack" address3 Nothing []
```

Lenses rely heavily on operators, although there are regular names for some of
them. The main ones we'll use are:

- ^. (view): gets the value focused on by the given lens.
- .~ (set): sets the value focused on by the given lens.
- %~ (modify): applies the given function to the value focused on by the lens.
- . : standard composition operator, allows composing lenses.
- & : this operator, which represents reverse function application, works mostly
  as a utility, and allows performing multiple set operations to a single
  target.

With that in place, we can start testing some lenses.

In these examples, we start _viewing_ into an instance with a given lens. We can
dig deeper by composing with other lens (`homeAddress` comes from Person, while
`doorNumber` comes from Address).

```haskell
print $ address1 ^. street
print $ person1 ^. homeAddress
print $ person1 ^. homeAddress . doorNumber
```

We can also update fields, even if they are nested:

```haskell
print $ person1
  & homeAddress . street .~ "Road 3"
  & homeAddress . doorNumber .~ 777
```

#### Notes on creating lenses

I personally prefer using:

```haskell
data Person = Person {
  _personName :: Text,
}

data Country = Country {
  _countryName :: Text,
}

makeLensesWith abbreviatedFields 'Person
makeLensesWith abbreviatedFields 'Country
```

It strips the prefix as long as it's lowercase (including the first underscore),
which eliminates the chances of having a name clash if you want to use the same
name of different types. In  the example, you can use `person ^. name` and
`country ^. name` without problems (the lens package takes care of this!).

### Usage in widgets

In these examples, the lens is what comes after the **^.** symbol. When using a
widget that receives a lens, that part is what we need to provide (not the
instance nor the _view_ operator).

## Prisms

What happens if you want to get a value wrapped in Maybe or Either? These kind
of types (sum types) are handled by Prisms, a different type of Optic.

The example shows, first, accessing a Maybe value. This is a regular lens, since
it fulfills the requirements of existing and being just one.

Then it shows an example that won't compile. Since a Maybe field can either be
_Just val_ or _Nothing_, you need to indicate which path you want to take (the
prism part comes from choosing which side of the prism you will use); this is
what happens in the third example.

You will also notice a new operator, `^?` (preview). Given that Maybe has two
options and we chose _Just, we have to account for the case where the value
contained in workAddress is actually Nothing. This operator will return Nothing
in case the prism (_Just) fails.

```haskell
print $ person1 ^. workAddress
-- print $ person1 ^? workAddress . doorNumber -- Does not compile
print $ person1 ^? workAddress . _Just . doorNumber
```

To update a Maybe field with a Just value, you can use any of these two options:

```haskell
print $ person2
  & workAddress .~ Just address1
print $ person2
  & workAddress ?~ address1
```

Compared to lenses, the guarantee of existence is removed, but we still know we
can get a maximum of one item.

### Usage in widgets

In general you can't use Prisms with Monomer's widgets, since they expect to be
able to get and set a single value, thus requiring a lens.

Having said that, some widgets such as numericField, dateField and timeField
have direct support for Maybe values. Others, such as radio or dropdown, receive
the set of options and you can wrap the valid values with Just, while also
remembering that Nothing is a valid value too and can be used to represent an
empty selection.

It's also valid to use prisms when updating your model in event handlers.

## Indexed

Finally, the last optics we'll explore are indexed optics. They allow getting
and setting values in lists/arrays/sequences based on an index. This type of
optic can fail, requiring the use of operators that account for that possible
failure.

To access the nth item of a sequence, we can use `ix`. Since the index may be
out of bounds, `^?` is used. An alternative is using `^?!`, which will return
the value if it exists, or raise an exception if it does not.

```haskell
print $ person2 ^? friends . ix 0 . homeAddress
print $ person2 ^?! friends . ix 0 . homeAddress
```

You can also update indexed optics. If the index does note exist, the update
will fail, but an exception will not be raised.

```haskell
print $ person2
  & friends . ix 0 .~ person3
print $ person2
  & friends . ix 10 .~ person3
```

### Usage in widgets

You can't use indexed optics with Monomer widgets for the same reason as prisms,
but you can convert them to lenses if you are sure you are within bounds.

#### singular

By wrapping the `ix 0` call in `singular`, you convert the indexed optic into a
lens. If the index is out of bounds it will raise an exception.

```haskell
print $ person2 ^. friends . singular (ix 0) . homeAddress
```

This is useful if, for instance, you have a list of editable items displayed all
at once in the screen. An example of this can be seen in
[Life Cycle](../03-life-cycle.md).

## Resources

If you want to dig deeper in optics, in particular the lens package, these
resources are recommended:

- [Official tutorial](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)
- [Haskell Lens Operator Onboarding](https://medium.com/@russmatney/haskell-lens-operator-onboarding-a235481e8fac)
- [Exercises for understanding lenses](https://williamyaoh.com/posts/2019-04-25-lens-exercises.html)
- [Optics By Example](https://leanpub.com/optics-by-example)

## Examples

You can run the example with:

```bash
stack runghc examples/tutorial/external/External01_Lenses.hs
```
