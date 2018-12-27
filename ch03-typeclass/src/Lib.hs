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

newtype C a = C { getC :: a }  deriving (Show,Eq)

instance Functor C where
    fmap f (C a) = C (f a)

instance Applicative C where
    pure = C 
    (C f) <*> (C a) = C (f a)

instance Monad C where
    return = C
    (C a) >>= f = f a

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Birds = Int
type Pole = (Birds,Birds)

pole :: Pole
pole = (0,0)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

checkPole :: Pole -> Maybe Pole
checkPole as@(left,right) = if abs(left-right) > 4 
                               then Nothing 
                               else Just as

landL :: Birds -> Pole -> Maybe Pole
landL n = checkPole . landLeft n

landR :: Birds -> Pole -> Maybe Pole
landR n = checkPole . landRight n


newtype Writer w a = Writer { runWriter::(a,w)}

instance Functor (Writer w) where
    fmap f (Writer (a,w)) = Writer (f a,w)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a,mempty)
    (Writer (f,v)) <*> (Writer (a,u)) = Writer (f a, mappend v u)

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a,mempty)
    (Writer (x,v)) >>= f = let (Writer (y,u)) = f x in Writer (y, mappend v u)


logNum :: Int -> Writer [String] Int
logNum x = Writer (x,["getNum " ++ show x])

telLog :: String -> Writer [String] ()
telLog s = Writer ((),[s]) 

mulitLog :: Writer [String] Int
mulitLog = do
    a <- logNum 4
    b <- logNum 5
    telLog "add telLog"
    return $ a * b
