{-# LANGUAGE DefaultSignatures, TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module KNXd.Client.Internal where

import Data.Serialize
import Data.Singletons
import KNXd.Client.Internal.TH
import KNXd.Client.Internal.Types
import KNXd.Client.Internal.PacketArgs
import KNXd.Client.Internal.Serialize

-- |When receiving a packet, we don't know what 'PacketType' it's indexed by.
-- But we do know where it came from.
data WireKnxPacket d s where
  WireKnxPacket :: PacketC d s t => KnxPacket d s t -> WireKnxPacket d s

deriving instance Show (WireKnxPacket d s)

getPacketType :: KnxPacket d s t -> PacketType
getPacketType (KnxPacket _ _ st _) = fromSing st

-- |Witness that a (d,s,t)-combination is valid (has an appropriate PacketC instance)
data PacketProof d s where
  PacketProof :: PacketC d s t => Sing (t :: PacketType) -> PacketProof d s

packetProof :: Sing (d :: PacketDirection)
            -> Sing (s :: ConnectionState)
            -> Sing (t :: PacketType) -> Maybe (PacketProof d s)
packetProof sd ss st = $(proofCases [|sd|] [|ss|] [|st|] [|Just $ PacketProof st|] [|Nothing|])
                                   
instance (SingI d, SingI s) => Serialize (WireKnxPacket d s) where
  put (WireKnxPacket packet) =
    putNested (putWord16be . fromIntegral) $ do
      putWire . fromPacketType . getPacketType $ packet
      putWire $ toWire packet
    
  get = getNested (fromIntegral <$> getWord16be) $ do
    --todo: instance of Serialize for packetType to avoid error
    t <- toPacketType <$> getWord16be
    let sd = sing :: Sing d
    let ss = sing :: Sing s
    case toSing t of
      SomeSing st -> case packetProof sd ss st of
        Just (PacketProof (st' :: Sing t')) -> do
          packet <- fromWire sd ss st' <$> getWire
          return $ WireKnxPacket packet
        Nothing -> fail "Received a packet I don't have a description for. Internal error."
