{-# LANGUAGE TemplateHaskell #-}

import Lib
import Data.List
import Test.QuickCheck

prop_minimum xs         = not (null xs) ==> head (qsort xs) == minimum xs
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs      = sort xs == qsort xs

--------------------------
return []

-- main = runTests

anal :: Args
anal = Args
    { replay = Nothing
    , maxSuccess = 1000
    , maxDiscardRatio = 1
    , maxSize = 1000
    , chatty = True
    , maxShrinks = 1
    }

minimal :: Args
minimal = Args
    { replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 1
    , maxSize = 200
    , chatty = True
    , maxShrinks = 1
    }

runTests :: Args -> IO Bool
runTests args = $forAllProperties $ quickCheckWithResult args

main :: IO Bool
main = do
    putStrLn "Choose test depth"
    putStrLn "1. Anal"
    putStrLn "2. Minimal"
    runTests minimal
    -- depth <- readLn
    -- if depth == 1
    --     then runTests anal
    -- else runTests minimal
