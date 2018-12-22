module Lib where

data Point = Point Float Float 

instance Show Point where
    show (Point x y) = "Point x = " ++ show x ++ " y = "++ show y


instance Eq Point where
    (Point x1 y1) ==  (Point x2 y2) = x1 == x2 && y1 == y2


-- yesno class type
--

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo Point where
    yesno (Point 0.0 0.0) = False
    yesno (Point _ _) = True

yesnoIf :: (YesNo y) => y -> a ->a ->a
yesnoIf y yes no = if yesno y then yes else no

someFunc :: IO ()
someFunc = putStrLn "someFunc"
