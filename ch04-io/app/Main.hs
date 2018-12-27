module Main where

import Data.Char
import Lib

main :: IO ()
main = someFunc

hello :: IO ()
hello = do
    putStrLn "what is your name"
    name <- getLine
    putStrLn $ "hello! " ++ name 

askName :: IO ()
askName = do
    putStrLn "what is your first name"
    firstName <- getLine
    putStrLn "what is your last name"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hello " ++ bigFirstName ++ " " ++ bigLastName ++ " !"
