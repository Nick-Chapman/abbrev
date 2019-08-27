{-# LANGUAGE LambdaCase #-}

module Attempt5(match) where

import Data.Char

--import Debug.Trace
trace :: String -> a -> a
trace _ a = a

match :: A -> B -> Bool
match a b =
    case chunkFromRight (reverse a) (reverse b) [] of
        Nothing -> False
        Just (pa,pb,chunks) -> loop pa pb chunks

loop :: PA -> PB -> Chunks -> Bool
loop pa pb chunks0 =
    trace (show ("loop",pa,pb, chunks0)) $
    case chunks0 of
    [] -> case matchInner pa pb of Nothing -> False; Just _ -> True
    (u,a,b):chunks ->
        case findUpper u pb of
            Nothing ->
                case matchInner pa pb of
                    Nothing -> False
                    Just _ -> loop a b chunks
            Just (ppb,pb') ->
                case matchInner pa ppb of
                    Nothing -> False
                    Just pa' -> greedy pa' pb' u a b chunks

greedy :: PA -> PB -> U -> A -> B -> Chunks -> Bool
greedy pa pb u a b chunks =
    trace (show ("greedy",(pa,a),(pb,u,b))) $
    case findUpper u pb of
        Nothing ->
            case matchInner pa pb of
                Nothing -> loop a (pb++[u]++b) chunks
                Just _ -> loop a b chunks

        Just (ppb,pb') ->
            case matchInner pa ppb of
                Nothing -> loop a (pb ++ [u] ++ b) chunks
                Just pa' -> greedy pa' pb' u a b chunks

matchInner :: PA -> PB -> Maybe PA
matchInner pa pb = case (pa,pb) of
    (_,[]) -> Just pa
    ([],_:_) -> Nothing
    (a1:pa',b1:pb') -> if matchChar a1 b1
                       then matchInner pa' pb'
                       else matchInner pa' pb

matchChar :: L -> U -> Bool
matchChar a b = do toUpper a == b

type Chunks = [(U,A,B)]

chunkFromRight :: A -> B -> Chunks -> Maybe (A,B,Chunks)
chunkFromRight a b chunks =
    case nextUpper a of
        Nothing -> Just (reverse a,reverse b,chunks)
        Just (pa,u,a') ->
            case findUpper u b of
                Nothing -> Nothing
                Just (pb,b') ->
                    chunkFromRight a' b' ((u,reverse pa,reverse pb):chunks)

nextUpper :: A -> Maybe (PA,U,A)
nextUpper = \case
    [] -> Nothing
    a1:a -> if isUpper a1 then Just ([],a1,a) else do
        (pa,u,a') <- nextUpper a
        return (a1:pa,u,a')

findUpper :: U -> B -> Maybe (PB,B)
findUpper u = \case
    [] -> Nothing
    b1:b -> if b1 == u then Just ([],b) else do
        (pb,b') <- findUpper u b
        return (b1:pb,b')

type A = String
type B = String
type L = Char
type U = Char
type PA = String
type PB = String
