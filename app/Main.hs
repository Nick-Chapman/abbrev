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
import qualified Attempt5
import qualified AttemptG

main :: IO ()
main = do
    args <- getArgs
    match <- evaluate $
            case args of
                ["1"] -> Attempt1.match -- timeouts in set 8
                ["2"] -> Attempt2.match -- timeouts in set 12
                ["3"] -> Attempt3.match -- timeouts in set 13
                ["4"] -> Attempt4.match -- passes everything
                ["5"] -> Attempt5.match -- fast, but broken on set 12/13
                ["G"] -> AttemptG.match -- Gary's (fastest, working)

                [] -> Attempt4.match -- my best
                _ -> error $ show args
    let alg =  uncurry match
    _tests4 <- loadTests "4" "tests4" tests4x
    _tests8 <- loadTests "8" "tests8" tests8x
    _tests12 <- loadTests "12" "tests12" tests12x
    _tests13 <- loadTests "13" "tests13" tests13x
    let allTests = []
            <> _myTests
            <> _tests4
            <> _tests8
            <> _tests12
            -- <> take 1 (drop 3 _tests12)
             <> _tests13
    runTests alg allTests (const True)

type AbbvCase = Case (String,String) Bool

_myTests :: [AbbvCase]
_myTests = [
    Case "my.1" ("thIs","THAT") False,
    Case "my.2" ("foo","FO") True,
    Case "my.3" ("axzayazaxazayaxaza","XYZ") True,
    Case "my.4" ("iaizixizayaiXiziaiyiaixibiYiciZixidiziyiai","ZXZAYAXBYCZXDZYA") True,

    -- Driving the code paths of Attempt 5...
    Case "my.5" ("axaxajaXaJaxa","AXAXAJAXA") True,
    Case "my.6" ("axaxajaXaJaxa","AXAXABJAXA") False,
    Case "my.7" ("axaxajaXaJaxa","ABXAXAJAXA") False,
    Case "my.8" ("axXaxaJaxa","AXAXAJAXA") True,
    Case "my.9" ("axXaaJaxa","AXAXAJAXA") False,

    -- Attempt-5 (A5)  fails on 12.4. Should be YES
    -- These are the head chunks determined by the A5 algorithm...
    -- ("loop","","ERR",[('E',"er",""),('R',"rrree","EEEEEE"),('R',"eee","E"),('E',"r","RR"),('E',"","")
    -- from which we can construct a much reduced testcase:
    Case "cut-12.4" ("EerRrrreeReeeErE", "ERREREEEEEEREERRE") False --True

    -- If the chunking algorithm was valid, the testcase should be a YES
    -- but it is clearly not a YES!
    -- which leads me to doubt the chunking-from-rear can be used.
    -- But why?

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
