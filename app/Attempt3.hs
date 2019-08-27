{-# LANGUAGE LambdaCase #-}

module Attempt3(match) where

import Data.Char

--import Debug.Trace
trace :: String -> a -> a
trace _ a = a

match :: A -> B -> Bool
match a b = trace (show ("match",a,b)) $
    case nextUpperInA a of
        Nothing -> case matchInner a b of Nothing -> False; Just _ -> True
        Just (pa,u,a') ->
            case findUpperInB u b of
                Nothing -> False
                Just (pb,b') ->
                    case matchInner pa pb of
                        Nothing -> False
                        Just pa' ->
                            greedyMatch pa' u a' b'

greedyMatch :: PA -> U -> A -> B -> Bool
greedyMatch pa u a b = trace (show ("greedy",pa,u,a,b)) $
    case findUpperInB u b of
        Nothing -> match a b
        Just (pb,b') ->
            case matchInner pa (u:pb) of
                Nothing -> match a b
                Just pa' ->
                    greedyMatch pa' u a b'
                    || match a b

nextUpperInA :: A -> Maybe (PA,U,A)
nextUpperInA = \case
    [] -> Nothing
    a1:a -> if isUpper a1 then Just ([],a1,a) else do
        (pa,u,a') <- nextUpperInA a
        return (a1:pa,u,a')

findUpperInB :: U -> B -> Maybe (PB,B)
findUpperInB u = \case
    [] -> Nothing
    b1:b -> if b1 == u then Just ([],b) else do
        (pb,b') <- findUpperInB u b
        return (b1:pb,b')

matchInner :: PA -> PB -> Maybe PA
matchInner pa pb = case (pa,pb) of
    (_,[]) -> Just pa
    ([],_:_) -> Nothing
    (a1:pa',b1:pb') -> if matchChar a1 b1
                       then matchInner pa' pb'
                       else matchInner pa' pb

matchChar :: L -> U -> Bool
matchChar a b = do toUpper a == b

type A = String
type B = String
type L = Char
type U = Char
type PA = String
type PB = String
