
{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Data.Char
--import Data.List as List
--import Control.Monad

main :: IO ()
main = do
    --run "axzayazaxazayaxaza" "XYZ"
    run "iaizixiziXiziaiyiaixibiYiciZixidiziyiai" "ZXZAYAXBYCZXDZYA"
    --run "iaizixizayaiXiziaiyiaixibiYiciZixidiziyiai" "ZXZAYAXBYCZXDZYA"
    --run "iaizixizayaiXiziaiyiaixibiYiqiZixidiziyiai" "ZXZAYAXBYCZXDZYA"
    --run "iaizixizayaiXiziaiyiaixibiYiciQixidiziyiai" "ZXZAYAXBYCZXDZYA"

run :: String -> String -> IO ()
run a b = do
    putStrLn $ "run: " <> show (a,b) <> "..."
    let aChunks = stage1 a
    let points = map fst (snd aChunks)
    let bChunks = stage2 b points
    print aChunks
    print bChunks

    --let res = abbreviation a b
    --putStrLn $ "run: " <> show (a,b) <> " -> " <> res

stage1 :: String -> (String, [(Char,String)])
stage1 s = do
    let (low1,back) = span isLower s
    case back of
        [] -> (low1,[])
        up:rest -> let (low2,chunks) = stage1 rest in (low1, (up,low2):chunks)

stage2 :: String -> String -> Maybe [String]
stage2 b points = do
    (res,left) <- stage2rev (reverse b) (reverse points)
    return $ map reverse (left : reverse res)

stage2rev :: String -> String -> Maybe ([String],String)
stage2rev s points = case points of
    [] -> Just ([],s)
    p1:points' -> do
        (xs,s') <- chop p1 s
        (res,left) <- stage2rev s' points'
        return ((xs++[p1]) : res, left)

chop :: Char -> String -> Maybe (String,String)
chop c s = do
    let (front,back) = span (/=c) s
    case back of
        [] -> Nothing
        _:rest -> Just (front,rest)


    {-
_abbreviation :: String -> String -> String
_abbreviation a b = if match a b then "YES" else "NO"

match :: String -> String -> Bool
match = undefined
-}
