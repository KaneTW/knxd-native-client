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
  (FamilyI (ClosedTypeFamilyD _ _ _ famEqns) _) <- reify ''PacketArgs
  caseE (tupE [dExp, sExp, tExp]) $ map mkMatch famEqns
  where
    mkMatch eqn = match (mkPat eqn) (normalB $ realBody eqn) []
    mkPat eqn = tupP . map typeToPat $ getTypes eqn
    getTypes (TySynEqn ts _) = ts
    typeToPat (ConT n) = conP (mkName $ 'S' : nameBase n) []
    typeToPat (PromotedT n) = conP (mkName $ 'S' : nameBase n) []
    typeToPat (VarT _) = wildP
    typeToPat t = error $ "Unsupported type " ++ show t
    
    realBody (TySynEqn [VarT _, VarT _, VarT _] _) = wBody
    realBody _ = body
  
