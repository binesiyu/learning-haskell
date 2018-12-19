module Lib where
    -- ( TestDataEnem(..)
    -- , TestDataPoint(..)
    -- , Person(..)
    -- , testPerson
    -- , arge'
    -- , height'
    -- ,someFunc
    -- ) where

data TestDataEnem = EnemOne | EnemTwo deriving (Show,Eq)
data TestDataPoint = TestDataPoint TestDataEnem | TestDataNorthing deriving (Show,Eq)

type FirstName = String
type LastName = String

data Person = Person { firstName :: FirstName
                      ,lastName :: LastName
                      ,arge :: Int
                      ,height :: Float
                     }deriving (Show)

newtype PersonNew = PersonNew Person deriving(Show)

testPerson = Person "fname" "lname" 39 170

firstName' :: Person -> FirstName
firstName' (Person name _ _ _) = name

lastName' :: Person -> LastName
lastName' (Person _ name _ _) = name

arge' :: Person -> Int
arge' (Person _ _ x _) = x

height' :: Person -> Float
height' (Person _ _ _ h) = h

data PersonG a b c = PersonG {firstNameG :: a
                         ,lastNameG ::a
                         ,argeG :: b
                         ,heightG :: c
                         }deriving (Show)

type PersonString = PersonG String Int Float
type PersonString' = PersonG String Integer Float

testPersonString1 :: PersonString
testPersonString1 = PersonG "1firstName" "1lastName" 35 178.0

testPersonString2 :: PersonString'
testPersonString2 = PersonG "2firstName" "2lastName" 35 178.0

someFunc :: IO ()
someFunc = putStrLn "someFunc"
