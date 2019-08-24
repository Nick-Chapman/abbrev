{-# LANGUAGE LambdaCase #-}

module Attempt2(match) where

import Data.Char(isUpper,toUpper)
import Data.List(isSuffixOf)

match :: String -> String -> Bool
match xs ys = not (null (solutions xs ys))

data Act = Drop | Keep | Convert
type Choice = (Char,Act)
type Cand = [Choice]

solutions :: String -> String -> [Cand]
solutions a b =
    filter good (mkCands a)
  where
    good = (== b) . eval

    ok = (`isSuffixOf` b) . eval

    mkCands :: String -> [Cand]
    mkCands s = foldr extend' [[]] s

    extend' :: Char -> [Cand] -> [Cand]
    extend' x cands = filter ok $ extend x cands

extend :: Char -> [Cand] -> [Cand]
extend x cands = do
    choice <- charChoices x
    cand <- cands
    return $ choice:cand

charChoices :: Char -> [Choice]
charChoices c = if isUpper c then [(c,Keep)] else [(c,Drop),(toUpper c,Convert)]

eval :: Cand -> String
eval = (>>= evalChoice)
    where
        evalChoice :: Choice -> String
        evalChoice (c,act) = case act of
            Drop -> ""
            Keep -> [c]
            Convert -> [c]
