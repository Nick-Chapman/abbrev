{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main(main) where

import Control.Exception (evaluate)
import System.Environment(getArgs)
import Testing(Case(..),runTests)

import qualified Attempt1
import qualified Attempt2
import qualified Attempt3
import qualified Attempt4

main :: IO ()
main = do
    args <- getArgs
    match <- evaluate $
            case args of
                ["1"] -> Attempt1.match
                ["2"] -> Attempt2.match
                ["3"] -> Attempt3.match
                ["4"] -> Attempt4.match
                [] -> Attempt4.match
                _ -> error $ show args
    let alg =  uncurry match
    _tests4 <- loadTests "4" "tests4" tests4x
    _tests8 <- loadTests "8" "tests8" tests8x
    _tests12 <- loadTests "12" "tests12" tests12x
    _tests13 <- loadTests "13" "tests13" tests13x
    let allTests =
            myTests
            <> _tests4
            <> _tests8
            <> _tests12
            <> _tests13
    runTests alg allTests (const True)

type AbbvCase = Case (String,String) Bool

myTests :: [AbbvCase]
myTests = [
    Case "my.1" ("thIs","THAT") False,
    Case "my.2" ("foo","FO") True,
    Case "my.3" ("axzayazaxazayaxaza","XYZ") True,
    Case "my.4" ("iaizixizayaiXiziaiyiaixibiYiciZixidiziyiai","ZXZAYAXBYCZXDZYA") True
    ]

tests4x :: [Bool]
tests4x = [y,n,y,y,n,y,y]

tests8x :: [Bool]
tests8x = [y,y,y,y,n,y,y,n,n,y]

tests12x :: [Bool]
tests12x = [y,n,y,y,y,n,n,n,n,y]

tests13x :: [Bool]
tests13x = [y,n,y,n,n,n,y,y,n,y]

y,n::Bool
(y,n) = (True,False)

loadTests :: String -> FilePath -> [Bool] -> IO [AbbvCase]
loadTests tag filename bools = do
    contents <- readFile filename
    return $ zipWith make [1::Int ..] $ zip (pairUp $ tail $ lines contents) bools
        where make i (input,expected) = Case { name = tag <> "." <> show i, input, expected }
              pairUp = \case a:b:list -> (a,b) : pairUp list; _ -> []
