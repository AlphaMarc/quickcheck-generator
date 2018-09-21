import Lib
import Test.QuickCheck
import Control.Monad
import Data.Random.Normal
import System.Random

firstNames = ["Cammy","Lakendra","Brock","Clarisa","Pok","Angelica","Kasandra","Bonita","Silvia","Trudy","Hae","Reyna","Dian","Clifford","Monica","Brianna","Jannette","Daysi","Rubye","Sherrie","Shaunda","Leandro","Odell","Earlene","Janell","Georgia","Deb","Delila","Elenora","Amal"]
lastNames = ["Hernandez", "Newton", "Pesola"]
-- test arguments
test_args = stdArgs

prop_FirstNameUnderTenChar :: Person -> Bool
prop_FirstNameUnderTenChar x = (length . firstName $ x) <= 10

-- returns either vanilla or chocolate or Strawberry with the same chance
genFlavour :: Gen String
genFlavour = oneof [return "Vanilla", return "Chocolate", return "Strawberry"]

-- returns age from 0 to 100
genAge :: Gen Int
genAge = choose (0,100)

-- returns a name
genName :: [String] ->  Gen String
genName = oneof . map return

genSize :: Gen Float
genSize = elements . take 1000  $ normals' (175.0, 12.5) (mkStdGen 0)

genPhone :: Gen String
--genPhone = vectorOf 10 (choose ('0', '9'))
genPhone = do
  zero <- return '0'
  one <- elements ['5', '6', '7']
  rest <- vectorOf 8 (choose ('0', '9'))
  return ([zero, one] ++ rest)

-- dummy instance always returns the same static info
--instance Arbitrary Person where
--  arbitrary = return (Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate")

instance Arbitrary Person where
  arbitrary = do
    flavour <- genFlavour
    age <- genAge
    firstName <- genName firstNames
    lastName <- genName lastNames
    number <- genPhone
    size <- genSize
    return (Person firstName lastName age size number flavour)





main :: IO ()
main = verboseCheck prop_FirstNameUnderTenChar
