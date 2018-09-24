# quickcheck-tuto

This little repo is used to showcase how one can use existing functions in the quickcheck library to generate an Arbitrary instance for their custom types and use this powerful library to test the properties of their code.

For a custom record type `Person` which represents a Person is defined in `Lib.hs` as
```
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float          -- Height in cm
                     , phoneNumber :: String
                     , flavor :: String         -- Favourite ice-cream Flavour
                     } deriving (Show)
``` 
We generate a cusom `Arbitrary` instance so that we can use that type in our QuickCheck tests. Extending the Arbitrary type class only needs implementing the arbitrary function which returns a `Gen Person`. 

First and Last names and Flavour are randomly picked amongst lists.
Age is a random number between 0 and 100.
Height is following a normal distribution to generate a random number.
Phone number is generating a French mobile phone number starting with 0 then either 6 or 7 then 8 random digits.


