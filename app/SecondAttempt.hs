{-# LANGUAGE LambdaCase #-}

module SecondAttempt(main) where

import Data.Char
import Data.List
import Control.Monad

main :: IO ()
main = _main2

_main1 :: IO()
_main1 = do
    q <- readLn :: IO Int
    forM_ [1..q] $ \_q_itr -> do
        a <- getLine
        b <- getLine
        let result = abbreviation a b
        putStrLn result

_main2 :: IO ()
_main2 = do
    run "axzayazaxazayaxaza" "XYZ"

    run "iaizixizayaiXiziaiyiaixibiYiciZixidiziyiai" "ZXZAYAXBYCZXDZYA"

    --run "aaaaAaabaaAaaaaabaaaAaaaaaaaa" "ABAAA"

    --run "CSHUWAVCQDYNONVTNECRWFQRGCVpFENJkRLYTKpiDQEJjKRJOIOEITAKQTQGUTIZDUERXSPPTSKZXsVMN" "CSHUWAVCQDYNONVTNECRWFQRGCVFENJRLYTKDQEJKRJOIOEITAKQTQGUTIZDUERXSPPTSKZXVMNJUN"

    --run "OPZFFVQLADBQFBXLOSUMZZWQUKASCUVQZZVWfPIRTytlvpijddqegbwitkhhsbuehtnpndvcandzjzyepvlnkayfkwzegvbratvwezddjqxrxocqgcghuohlmsondvicocltqhvqfqjpctxfomjoukrheijhhndcbipiobvpbskemgykepokluwqhhejdaimvdvlegfyrrwckgojsbsxmsvhhrlnvcrxfaxinjzsjgvvrlcczqlkvgtftsvktvhtfpaklumhkovphilrappbvkarfhvwxxtrugypracozyqyvaqjityoiyemyavpbchaoagrvujocpueczsgcqdjvkjckxhmnaseshjgecusrxozuxgeieleewwskmiprlqnshvmcp" "OPZFFVQLADBQFBXLOSUMZZWQUKASCUVQZZVWPIRT"

    -- tests8, #6, YES
    --run "VUWELCNJMNWLMJLZRASXaZCTBXKLLELZNWNZXNBTAPKRBBsXBJHMBDPDQDIFCXHXWNVMTFHSNAJhRSUAIAXLNICSBCIOLOAMAOAPGJVXEFBGEFCKQzMAFTVZKMGIXEKVWMbQPZTFHVLSQGBXEaFRKAMMICCGDPXWGZTGJWRCRBQIpCRBIAYRDXLMWNGEUMELKAZANQBLKTTVKQJOSZRNHUJBNDFTNFJVUNrGWKWALLBERYEgXMSXRMWHKQIFRQELUHOFGVyLESCNBWOSTOPRQYIDDTWNUCrBOOUMTLKNDRXTDPGQQERPFRJQEGEFLDUayvvmqaaypkxezuhsopxexsnfdaxc" "VUWELCNJMNWLMJLZRASXZCTBXKLLELZNWNZXNBTAPKRBBXBJHMBDPDQDIFCXHXWNVMTFHSNAJRSUAIAXLNICSBCIOLOAMAOAPGJVXEFBGEFCKQMAFTVZKMGIXEKVWMQPZTFHVLSQGBXEFRKAMMICCGDPXWGZTGJWRCRBQICRBIAYRDXLMWNGEUMELKAZANQBLKTTVKQJOSZRNHUJBNDFTNFJVUNGWKWALLBERYEXMSXRMWHKQIFRQELUHOFGVLESCNBWOSTOPRQYIDDTWNUCBOOUMTLKNDRXTDPGQQERPFRJQEGEFLDU"

    -- tests8, #5, NO
    --run "CIVQEESyFYnGDSSUUUGMPXYUKRMLXRXtWAWKQRUWCXKBMTGDOWSPRFOCUOETTLIWeXTUHSSPWYQKJSIlRJGOIDARFIILFXQUBCXUQHJCtJXTJBOSJKJUAIFaBVQWBXWZIYRMYOCVYGTCJJjDMBAESZlXMDPIREZHVJGJQHAFQGGXLzIEAPcZGBOEHDXQIUDfBEYQOjTYJUJVTWEIXcBUYEyXHPDYAEHOZDPHAQAYEQNKoVBOOMTUOJHyFOLRmVKMwFVCJMTAMFVPAGYYIBZZLCPJYXLWXMHLVXXQOGSZKGZZOENOSNHJNOMXxNMRZGODIUnEZGRDFLNuZJASKXHMSJGIWGIUYWPPXQQZYDSISXFQRPLHFPHMZMGMVOLXeJWYZOZUEOHWZOFUQEGEGLPRISELSNHIGDlLqEDCCDJYKAFTLLPIYUQENFuWJJFHUAECO" "CIVQEESFYGDSSUUUGMPXYUKRMLXRXWAWKQRUWCXKBMTGDOWSPRFOCUOETTLIWXTUHSSPWYQKJSIRJGOIDARFIILFXQUBCXUQHJCJXTJBOSJKJUAIFBVQWBXWZIYRMYOCVYGTCJJDMBAESZXMDPIREZHVJGJQHAFQGGXLIEAPZGBOEHDXQIUDBEYQOTYJUJVTWEIXBUYEXHPDYAEHOZDPHAQAYEQNKVBOOMTUOJHFOLRVKMFVCJMTAMFVPAGYYIBZZLCPJYXLWXMHLVXXQOGSZKGZZOENOSNHJNOMXNMRZGODIUEZGRDFLNZJASKXHMSJGIWGIUYWPPXQQZYDSISXFQRPLHFPHMZMGMVOLXJWYZOZUEOHWZOFUQEGEGLPRISELSNHIGDLEDCCDJYKAFTLLPIYUQENFWJJFHUAECOMN"

    -- tests12, #1, YES
    --run "hHhAhhcahhacaccacccahhchhcHcahaahhchhhchaachcaCchhchcaccccchhhcaahhhhcaacchccCaahhaahachhacaahhaachhhaaaCalhhchaccaAahHcchcazhachhhaaahaahhaacchAahccacahahhcHhccahaachAchahacaahcahacaahcahacaHhccccaahaahacaachcchhahhacchahhhaahcacacachhahchcaAhhcaahchHhhaacHcacahaccccaaahacCHhChchhhahhchcahaaCccccahhcaachhhacaaahcaaaccccaacaaHachaahcchaahhchhhcahahahhcaachhchacahhahahahAahaAcchahaahcaaaaahhChacahcacachacahcchHcaahchhcahaachnachhhhcachchahhhacHhCcaHhhhcaCccccaaahcahacahchahcaachcchaachahhhhhhhhcahhacacCcchahccaaaaaHhhccaAaaaCchahhccaahhacaccchhcahhcahaahhgacahcahhchcaaAccchahhhaahhccaaHcchaccacahHahChachhcaaacAhacacaacacchhchchacchchcacchachacaahachccchhhaccahcacchaccaahaaaccccccaaaaaaaHhcahcchmcHchcchaaahaccchaaachchHahcaccaaccahcacacahAhaacaacaccaccaaacahhhcacAhaCchcaacCcccachhchchcchhchahchchahchchhchcacaachahhccacachaAhaaachchhchchchhaachahaahahachhaaaccacahhcacchhhaaachaaacAahhcachchachhhcacchacaaChCahhhccahChaachhcahacchanaaacchhhccacacchcahccchAcahacaaachhacchachccaaHacaacAhahcCh" "HAHHCHAACCCAHCHHAHHAHCACCHCCHHCAAHHCACCCAHHHACAAHHHHCHHCAHHAHHAAAHAACAAHAHHCAHAHACHACHCHACACHAAHHAAAHCAHHACACAACHHHCHAHCAHCHHHAHAHACCAAAHCHHCHHCCAACCCCAACHACAACAAHACHCHAHHACCHCAHHHAAACHACAACHCACACAHHCCHAHACCCACCAACHCHHHCCCCCHCCAHHCAAHHAHHHHHHHAACCCCAHCCAAAAAHHHAAAACCAHHCAHACACCHHCHAHAHHCHAACHHHHHCCHCCAHAHCHCAAACCACCCCHACCACHHACHHACACHACCAACCCCAAAAHHAHCHHHCCAHCCHACHHAHCCACACCHAHAAACACCCCAHCCAHACCCCCCHCCHHCHHHHCHCHCAHHHACHAHAACCCAAAACHAACAAAHHAAHAAAHACHHCACHCCHCHAACHACACHHCCCCCAHCACHAAAHCHCAHACAAC"


run :: String -> String -> IO ()
run = _run1

_run1 :: String -> String -> IO ()
_run1 a b = do
    putStrLn $ "run: " <> show (a,b) <> "..."
    let ss = solutions a b
    putStrLn $ "#solutions = " <> show (length ss)
    putStrLn $ a
    mapM_ (putStrLn . seeCandLine2) (take 10 ss)

seeCandLine2 :: Cand -> String
seeCandLine2 = map seeChoice
    where
        seeChoice (c,act) = case act of
            Drop -> '.'
            Keep -> c
            Convert -> toLower c

_run2 :: String -> String -> IO ()
_run2 a b = do
    putStrLn $ "run: " <> show (a,b) <> "..."
    let res = abbreviation a b
    putStrLn $ "run: " <> show (a,b) <> " -> " <> res

data Act = Drop | Keep | Convert
type Choice = (Char,Act)
type Cand = [Choice]

abbreviation :: String -> String -> String
abbreviation a b = if match a b then "YES" else "NO"

match :: String -> String -> Bool
match xs ys = not (null (solutions xs ys))

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
