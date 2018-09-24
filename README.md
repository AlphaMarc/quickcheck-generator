# quickcheck-tuto

Property based testing is the perfect addition to a pure functional language such as Haskell. Thanks to the use of generic types in Haskell it is possible to derive information about what a function does just by looking at its signature. For example there is only one function with the signature `f :: a -> a` and that is the identity function. Another one, when I see `g :: [a] -> [a]` I know that all the elements in the result list were in the one given as parameter. These properties are guaranteed by the compiler.

Let's take that second example, I can infer some property but that still doesn't quite tell me what that function does. Does it reverse the list? Does it returns the list without its first element? That's where property based testing comes in handy. If I add a couple properties to that function such as
``` 
g . g = id
g(x:xs) = g(xs) ++ [x]
```
Then I know that this function is no other than the reverse function. These properties however are not checked by the compiler. That's where libraries such as QuickCheck come in handy because it allows to write these properties and to test them against a number of randomly generated examples.

A lot has been written about property based testing and QuickCheck. However I could not find a simple example that leverages the functions in the quickcheck library to generate an `Arbitrary` instance for a custom record types.

We start by defining a custom record type `Person` which represents a Person in `Lib.hs` as
```
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float          -- Height in cm
                     , phoneNumber :: String
                     , flavor :: String         -- Favourite ice-cream Flavour
                     } deriving (Show)
```

QuickCheck comes with a handful of implementations to generate random values for common types. If you are using a custom record type you will need to extend the `Arbitrary` instance so that you can use that type in our QuickCheck tests. Extending the Arbitrary type class is only a matter of implementing the arbitrary function which returns a `Gen Person`.

In this example we show 4 examples of functions you might want to use:
 * First and Last names and Flavour are randomly picked amongst lists using the function `oneof :: [Gen a] -> Gen a`
``` 
genName :: [String] ->  Gen String
genName = oneof . map return
``` 
 * Age is a random number between 0 and 100. In that case we use the function `choose :: Random a => (a, a) -> Gen a`. Be wary that the type `a`must implement the Random typeclass (Integers implementation is given to us).
```
genAge :: Gen Int
genAge = choose (0,100)
```
 * Height is following a normal distribution to generate a random number. In order to generate sizes that make sense for our tests we will get a little help from the `Data.Random.Normal` package to generate numbers that follow a normal distribution (a good enough approximation of reality). For this one, we generate a list of 1000 numbers and we let the `elements`function pick one. This list of 1000 elements will always be the same everytime we run our tests because we initialise with a static seed. It could be improved with a dynamic seed or using the global random number generator.
``` 
genSize :: Gen Float
genSize = elements . take 1000  $ normals' (175.0, 12.5) (mkStdGen 0)
```

 * Phone number is generating a French mobile phone number starting with 0 then either 6 or 7 then 8 random digits. This function uses functions we explained above and `vectorOf :: Int -> Gen a -> Gen [a]`. This function generates a list of a given size using a given generator. In our case the generator is `choose ('0','9')` and the list size is 8. We finally wrap it up by concatenating the list of numbers and wrap it in the `Gen` monad using return.
```
genPhone :: Gen String
genPhone = do
  zero <- return '0'
  one <- elements ['5', '6', '7']
  rest <- vectorOf 8 (choose ('0', '9'))
  return ([zero, one] ++ rest)
 ```


