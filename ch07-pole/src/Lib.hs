module Lib where

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n , right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left , right + n)


x -: f = f x

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
  | abs ((left + n) - right) <= 3 = Just (left + n , right)
  | otherwise = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
  | abs ((right + n) - left) <= 3 = Just (left,right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- land :: Birds ->Birds -> Pole -> Maybe Pole
-- land l r (left,right)
--   | abs((left + l) - (right + r)) <= 3 = Just (left + l,right + r)
--   | otherwise = Nothing

land :: Birds ->Birds -> Pole -> Either String Pole
land l r (left,right)
  | abs((left + l) - (right + r)) <= 3 = Right (left + l,right + r)
  | otherwise = Left $ "drop " ++ show l

landLeft'' = flip land 0

landRight'' = land 0


someFunc :: IO ()
someFunc = putStrLn "someFunc"
