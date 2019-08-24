{-# LANGUAGE LambdaCase #-}

module Attempt1(match) where

import Data.Char as Char

match :: String -> String -> Bool
match xs ys = case ys of
    [] -> base0 xs
    [y] -> base1 xs y
    _ -> do
        let (ys1,ys2) = splitHalf ys
        any (\(xs1,xs2) ->
                 if length xs1 < length xs2
                 then match xs1 ys1 && match xs2 ys2
                 else match xs2 ys2 && match xs1 ys1
            ) (allSplits xs)

splitHalf :: String -> (String,String)
splitHalf xs = splitAt (length xs `quot` 2) xs

allSplits :: String -> [(String,String)]
allSplits xs = map (flip splitAt xs) [1..length xs - 1]

base0 :: String -> Bool
base0 = allLower

base1 :: String -> Char -> Bool
base1 s c =
    (allLower s && containedIn (Char.toLower c) s)
    || oneUpper s && getFirstUpper s == c

allLower :: String -> Bool
allLower = all Char.isLower

containedIn :: Char -> String -> Bool
containedIn c s = c `elem` s

oneUpper :: String -> Bool
oneUpper xs = (length $ filter isUpper xs) == 1

getFirstUpper :: String -> Char
getFirstUpper xs = head $ filter isUpper xs
