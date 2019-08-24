{-# LANGUAGE LambdaCase #-}

module Attempt4(match) where -- memoized version of Attempt3

import Data.Char
import Control.Monad.ST (ST,runST)
import qualified Data.HashTable.ST.Basic as H

match :: A -> B -> Bool
match a b = runST $ do
    m <- H.new
    m_match m a b

type K = (String,String)
type V = Bool
type Memo x = H.HashTable x K V

m_match :: Memo x -> A -> B -> ST x Bool
m_match m a b = do
    H.lookup m (a,b) >>= \case
        Just res -> return res
        Nothing -> do
            res <- m_match1 m a b
            H.insert m (a,b) res
            return res

m_match1 :: Memo x -> A -> B -> ST x Bool
m_match1 m a b =
    case nextUpperInA a of
        Nothing -> return $ case matchInner a b of Nothing -> False; Just _ -> True
        Just (pa,u,a') ->
            case findUpperInB u b of
                Nothing -> return False
                Just (pb,b') ->
                    case matchInner pa pb of
                        Nothing -> return False
                        Just pa' ->
                            m_greedyMatch m pa' u a' b'

m_greedyMatch :: Memo x -> PA -> U -> A -> B -> ST x Bool
m_greedyMatch m pa u a b =
    case findUpperInB u b of
        Nothing -> m_match m a b
        Just (pb,b') ->
            case matchInner pa (u:pb) of
                Nothing -> m_match m a b
                Just pa' -> do
                    b1 <- m_greedyMatch m pa' u a b'
                    if b1 then return True else m_match m a b

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
matchChar a b = toUpper a == b

type A = String
type B = String
type L = Char
type U = Char
type PA = String
type PB = String
