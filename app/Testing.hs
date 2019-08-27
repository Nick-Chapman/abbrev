{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testing (Case(..),runTests) where

import Control.Exception (evaluate,catch,Exception,SomeException)
import System.IO (stdout,hFlush)
import System.TimeIt(timeItT)
import System.Timeout (timeout)
import Text.Printf (printf)

secondsToTimeout :: Double
secondsToTimeout = 2.0

data Case a b = Case { name :: String, input :: a, expected :: b }
type Alg a b = a -> b
type Select = String -> Bool

runTests :: Show b => Eq b => Alg a b -> [Case a b] -> Select -> IO ()
runTests alg tests select = do
    let selected = filter (\Case{name} -> select name) tests
    results <- mapM (runTest alg) selected
    putStrLn $ totalize results

data Time = Time Double
instance Show Time where show (Time d) = printf "%.2f" d

data Result = Pass Time | Fail | Exception String | Timeout Time

instance Show Result where
    show = \case
        Pass time -> "pass (" <> show time <> ")"
        Fail -> "FAILURE"
        Exception s -> "EXCEPTION: " <> s
        Timeout n -> "TIMEOUT, after " <> show n <> " second."

totalize :: [Result] -> String
totalize rs = "totals: " <> show p <> "/" <> show n <> " pass"
    <> if f+x+t == 0 then "" else
           " (" <> show f <> " FAILs, " <> show x <> " EXCEPTIONs, " <> show t <> " TIMEOUTs)"
    where n = length rs
          p = length $ filter (\case Pass _ -> True; _ -> False) rs
          f = length $ filter (\case Fail -> True; _ -> False) rs
          x = length $ filter (\case Exception _ -> True; _ -> False) rs
          t = length $ filter (\case Timeout _ -> True; _ -> False) rs

runTest :: Show b => Eq b => Alg a b -> Case a b -> IO Result
runTest alg Case{name,input,expected} = do
    putStr $ name <> "..."; hFlush stdout
    let actual = alg input
    res <- runCheck secondsToTimeout (actual == expected)
    print res
    return res

runCheck :: Double -> Bool -> IO Result
runCheck n b = do
    classify <$> (catchAnything $ timeout mics $ timeItT $ evaluate b)
    where
        mics = truncate (n * fromIntegral (1000000::Int))
        classify = \case
            --Left e -> Exception (head $ lines $ show (e::SomeException))
            Left e -> Exception (show (e::SomeException))
            Right Nothing -> Timeout (Time n)
            Right (Just (_,False)) -> Fail
            Right (Just (d,True)) -> Pass (Time d)

catchAnything :: Exception e => IO a -> IO (Either e a)
catchAnything io = (Right <$> io) `catch` (return . Left)
