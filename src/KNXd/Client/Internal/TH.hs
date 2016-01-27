{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module KNXd.Client.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.List

defineToEnum :: QuasiQuoter
defineToEnum = QuasiQuoter { quoteDec = parseHeader }

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
parseHeader hdr = return [mkDataDecl fixedValues]
  where
    defines = filter ("#define" `isPrefixOf`) $ lines hdr
    values = map (\[_,k,v] -> (k,v)) $ filter (\x -> length x == 3) $ map words defines
    
    camelCase []     = []
    camelCase (x:xs) =  toUpper x : map toLower xs
    fixNames name = concatMap camelCase . tail $ splitOn' '_' name
    fixedValues = map (\(k,v) -> (fixNames k, read v :: Integer)) values

    mkDataDecl vals = let cons = map (\(k,_) -> NormalC (mkName k) []) vals
                      in DataD [] (mkName "ConnectionType") [] cons [mkName "Show"]
