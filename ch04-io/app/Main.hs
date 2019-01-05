module Main where

import Data.Char
import Lib
import System.Environment (getArgs)

main :: IO ()
main = countLine

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

countLine :: IO ()
countLine = interact worldcount
            where worldcount input = show (length (lines input)) ++ "\n"


interactWith function inputFile outFile = do
    input <- readFile inputFile
    writeFile outFile (function input)

mainargs = mainWith myFunction
    where mainWith function = do
                args <- getArgs
                case args of 
                  [input,output] -> interactWith function input output
                  _ -> putStrLn "error: exacly two argumests needed"
          myFunction = id
