module Lib where
    -- ( someFunc
    -- ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Pattern mathing
syeMe :: (Integral a) => a -> String
syeMe 1 = "one!"
syeMe 2 = "two!"
syeMe 3 = "three!"
syeMe 4 = "four!"
syeMe 5 = "five!"
syeMe _ = "Not between in 1 and 5"

-- Guards
syeMeGuards :: (Integral a) => a -> String
syeMeGuards x
      | x == 1 = "one!"
      | x == 2 = "two!"
      | x == 3 = "three!"
      | x == 4 = "four!"
      | x == 5  = "five!"
      | otherwise = "Now between in 1 and 5"

-- if
syeMeIf :: (Integral a) => a -> String
syeMeIf x = if x == 1
               then "one!"
               else
                if x == 2
                   then "two!"
                   else
                     if x == 3
                        then "three!"
                        else
                          if x == 4
                             then "four!"
                             else
                               if x == 5
                                  then "five!"
                                  else "Not between 1 and 5"


-- case
syeMeCase :: (Integral a) => a -> String
syeMeCase x = case x of 1 -> "one!"
                        2 -> "two!"
                        3 -> "three!"
                        4 -> "four!"
                        5 -> "five!"
                        _ -> "Not between 1 and 5"
-- where
syeMeWhere :: (Integral a) => a -> String
syeMeWhere x
  | x == 1 = one
  | x == 2 = two
  | x == 3 = three
  | x == 4 = four
  | x == 5  = five
  | otherwise = unknow
  where   one = "one!"
          two = "two!"
          three = "three!"
          four = "four!"
          five = "five"
          unknow = "Not between in 1 and 5"

-- where
syeMeWhere' :: (Integral a) => a -> String
syeMeWhere' x
  | x == 1 = one
  | x == 2 = two
  | x == 3 = three
  | x == 4 = four
  | x == 5  = five
  | otherwise = unknow
  where (one,two,three,four,five,unknow) = ("one!","two!","three!","four!","five!","Not between in 1 and 5!")

-- let..in
syeMeLet :: (Integral a) => a -> String
syeMeLet x = let  (one,two,three,four,five,unknow) = ("one!","two!","three!","four!","five!","Not between in 1 and 5!")
              in case x of
                        1 -> one
                        2 -> two
                        3 -> three
                        4 -> four
                        5 -> five
                        _ -> unknow

--Recursion
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (_:xs) = 1 + lengthList xs

lengthList' :: [a] -> Int
lengthList' [] = 0
lengthList' (_:xs) = lengthList xs + 1

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
    where less = [y | y <- xs, x >= y]
          more = [y | y <- xs, x < y]
