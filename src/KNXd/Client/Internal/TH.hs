{-# LANGUAGE TemplateHaskell #-}
module KNXd.Client.Internal.TH where

import Language.Haskell.TH

import KNXd.Client.Internal.Types

proofCases :: Q Exp -- ^ PacketDirection scrutinee
           -> Q Exp -- ^ ConnectionState s.
           -> Q Exp -- ^ PacketType s.
           -> Q Exp -- ^ body
           -> Q Exp
proofCases dExp sExp tExp body = caseE dExp =<< matchesDir
  where
    matchesDir = matches ''PacketDirection (caseE sExp =<< matchesState)
    matchesState = matches ''ConnectionState (caseE tExp =<< matchesType)
    matchesType = matches ''PacketType body
    matches name inner = do
      (TyConI (DataD _ _ _ cons _ )) <- reify name
      let names = map conName cons
      return $ map (\n -> match (conP n []) (normalB inner) []) names

    conName (NormalC n _) = mkName $ 'S' : (nameBase n)
    conName _ = error "Unsupported constructor"
