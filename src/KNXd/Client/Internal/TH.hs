{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module KNXd.Client.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.List

defineTypes :: QuasiQuoter
defineTypes = QuasiQuoter { quoteDec = parseHeader }

splitOn' :: Eq a => a -> [a] -> [[a]]
splitOn' = go [] 
  where
    go res _ [] = reverse res
    go res s ls = let (pre, suf) = break (==s) ls
                  in case suf of
                  []   -> go (pre:res) s []
                  [_]  ->  go ([]:pre:res) s []
                  -- invariant: x == s
                  _:xs -> go (pre:res) s xs

parseHeader :: String -> Q [Dec]
parseHeader hdr = return $ [mkToFunSig, mkFromFunSig] ++ map ($ fixedValues) [mkDataDecl, mkToFunDecl, mkFromFunDecl]
  where
    defines = filter ("#define" `isPrefixOf`) $ lines hdr
    values = map (\[_,k,v] -> (k,v)) $ filter (\x -> length x == 3) $ map words defines
    
    camelCase []     = []
    camelCase (x:xs) =  toUpper x : map toLower xs
    fixNames name = concatMap camelCase . tail $ splitOn' '_' name
    fixedValues = map (\(k,v) -> (fixNames k, read v :: Integer)) values

    dataName = mkName "PacketType"
    toName = mkName "toPacketType"
    fromName = mkName "fromPacketType"
    word16Con = ConT . mkName $ "Word16"

    mkDataDecl vals = let cons = map (\(k,_) -> NormalC (mkName k) []) vals
                      in DataD [] dataName [] cons [mkName "Show", mkName "Eq", mkName "Typeable"]

    mkToFunSig = SigD toName $ AppT (AppT ArrowT (ConT dataName)) word16Con
    mkFromFunSig = SigD fromName $ AppT (AppT ArrowT word16Con) (ConT dataName)
    
    mkToFunDecl vals = let cl (k,v) = Clause [RecP (mkName k) []] (NormalB . LitE . IntegerL $ v) []
                       in FunD toName $ map cl vals
    mkFromFunDecl vals = let cl (k,v) = Clause [LitP . IntegerL $ v] (NormalB . ConE  $ mkName k) []
                         in FunD fromName $ map cl vals ++ [Clause [WildP] (NormalB $ AppE (VarE $ mkName "error") (LitE $ StringL "unknown packet type")) []]


