{-# LANGUAGE TemplateHaskell #-}
module KNXd.Client.Internal.TH where

import Language.Haskell.TH

import KNXd.Client.Internal.PacketArgs

proofCases :: Q Exp -- ^ PacketDirection scrutinee
           -> Q Exp -- ^ ConnectionState s.
           -> Q Exp -- ^ PacketType s.
           -> Q Exp -- ^ body
           -> Q Exp -- ^ wildcard body
           -> Q Exp
proofCases dExp sExp tExp body wBody = do
  (FamilyI _ famDecs) <- reify ''PacketArgs
  let famEqns = map getEqn famDecs
  
  caseE (tupE [dExp, sExp, tExp]) $ map mkMatch famEqns ++ [wildcardMatch]
  where
    getEqn (TySynInstD _ eqn) = eqn
    getEqn d = error $ "Unsupported decl " ++ show d
    
    wildcardMatch = match [p|(_,_,_)|] (normalB wBody) []
    
    mkMatch eqn = match (mkPat eqn) (normalB body) []
    mkPat eqn = tupP . map typeToPat $ getTypes eqn
    getTypes (TySynEqn ts _) = ts
    typeToPat (ConT n) = conP (singName n) []
    typeToPat (PromotedT n) = conP (singName  n) []
    typeToPat (VarT _) = wildP
    typeToPat t = error $ "Unsupported type " ++ show t

    singName n = (mkName $ 'S' : nameBase n)
  
