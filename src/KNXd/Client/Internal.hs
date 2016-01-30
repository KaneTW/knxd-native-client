{-# LANGUAGE DefaultSignatures, TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module KNXd.Client.Internal where

import Data.ByteString (ByteString)
import Data.HList
import Data.Serialize
import Data.Singletons
import Data.Singletons.TH
import Data.Void
import Data.Word
import KNXd.Client.Internal.TH
import KNXd.Client.Internal.Types
import KNXd.Client.Internal.PacketArgs

-- |A KNX packet. Arguments are determined by it's type, see 'PacketArgs'
data KnxPacket (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType) where
  KnxPacket :: Sing d -> Sing s -> Sing t -> PacketArgs d s t -> KnxPacket d s t

-- |When receiving a packet, we don't know what PacketType it's indexed by.
-- But we do know where it came from.
data WireKnxPacket d s where
  WireKnxPacket :: (ConvertWire (WirePacketArgs d s t)
                 , (ConvertUnused (PacketArgs d s t) (WirePacketArgs d s t))
                 , (ConvertUnused (WirePacketArgs d s t) (PacketArgs d s t)))
                => KnxPacket d s t -> WireKnxPacket d s

-- |Type -> Term
getPacketType :: KnxPacket d s t -> PacketType
getPacketType (KnxPacket _ _ st _) = fromSing st

getPacketDirection :: KnxPacket d s t -> PacketDirection
getPacketDirection (KnxPacket sd _ _ _) = fromSing sd

-- |Default value for serializing unused fields
class DefaultValue a where
  defaultValue :: a

instance DefaultValue Word16 where
  defaultValue = 0

instance DefaultValue Bool where
  defaultValue = False

-- |Unused fields that need to be present on the wire but don't have any actual semantics
data Unused a where
  Unused :: DefaultValue a => Unused a

-- |Describes how each packet type looks on the wire
type family WirePacketArgs (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType) where
  WirePacketArgs 'ToServer 'Fresh 'OpenTConnection = HList '[IndividualAddress, Unused Bool]
  WirePacketArgs 'ToServer 'Fresh 'OpenTIndividual = HList '[IndividualAddress, Bool]
  WirePacketArgs 'ToServer 'Fresh 'OpenTGroup = HList '[GroupAddress, Bool]
  WirePacketArgs 'ToServer 'Fresh 'OpenTBroadcast = HList '[Unused Word16, Bool]
  WirePacketArgs 'ToServer 'Fresh 'OpenTTpdu = HList '[IndividualAddress, Unused Bool]
  WirePacketArgs 'ToServer 'Fresh 'OpenGroupcon = HList '[Unused Word16, Bool]
  -- and more...
  
  WirePacketArgs d s t = PacketArgs d s t

-- |Used to convert to/from wire representation
class ConvertUnused k l where
  convertUnused :: k -> l

instance {-# INCOHERENT #-} ConvertUnused (HList k) (HList k) where
  convertUnused = id

instance (DefaultValue el, ConvertUnused (HList k) (HList l))
         => ConvertUnused (HList k) (HList (Unused el ': l)) where
  convertUnused k = HCons Unused $ convertUnused k

instance ConvertUnused (HList k) (HList l)
         => ConvertUnused (HList (e ': k)) (HList (e ': l)) where
  convertUnused (HCons el k) = HCons el $ convertUnused k

instance ConvertUnused (HList k) (HList l)
         => ConvertUnused (HList (Unused el ': k)) (HList l) where
  convertUnused (HCons _ k) = convertUnused k

instance ConvertUnused Void Void where
  convertUnused = id


-- |Some types need nonstandard serialization, 
-- so it's easier to just serialize in a special class.
class ConvertWire a where
  putWire :: Putter a
  default putWire :: Serialize a => Putter a
  putWire = put

  getWire :: Get a
  default getWire :: Serialize a => Get a
  getWire = get

instance ConvertWire Word8 where
  putWire = putWord8
  getWire = getWord8

instance ConvertWire Word16 where
  putWire = putWord16be
  getWire = getWord16be

instance ConvertWire Word32 where
  putWire = putWord32be
  getWire = getWord32be

instance ConvertWire Bool where
  putWire True  = putWord8 0xff
  putWire False = putWord8 0
  
  getWire = (>0) <$> getWord8

-- |This instance assumes an isolate block and that every 'ByteString' comes last
instance ConvertWire ByteString where
  -- |Note that this uses putByteString to avoid writing length
  putWire = putByteString
  getWire = remaining >>= getByteString

instance ConvertWire e => ConvertWire [e] where
  putWire = mapM_ putWire
  getWire = do
    len <- remaining
    let count = len `div` 2
    replicateM count getWire

instance ConvertWire e => ConvertWire (Maybe e) where
  putWire = mapM_ putWire
  getWire = do
    len <- remaining
    case len of
      0 -> return Nothing
      _ -> Just <$> getWire

instance ConvertWire (HList '[]) where
  putWire _ = return ()
  getWire   = return HNil

instance (ConvertWire e, ConvertWire (HList l))
         => ConvertWire (HList (e ': l)) where
  putWire (HCons e l) = putWire e >> putWire l
  getWire = HCons <$> getWire <*> getWire

deriving instance ConvertWire IndividualAddress
deriving instance ConvertWire GroupAddress

instance ConvertWire ProgCommand where
  putWire = putWord8 . fromIntegral . fromEnum
  getWire = (toEnum . fromIntegral) <$> getWord8

instance (ConvertWire a, DefaultValue a) => ConvertWire (Unused a) where
  putWire Unused = putWire (defaultValue :: a)
  getWire = return Unused

instance ConvertWire Void where
  putWire = absurd
  getWire = fail "Bug in packet description. This should never happen."

data ConvertibleProof d s where
  ConvertibleProof :: (ConvertWire (WirePacketArgs d s t)
                   , (ConvertUnused (WirePacketArgs d s t) (PacketArgs d s t))
                   , (ConvertUnused (PacketArgs d s t) (WirePacketArgs d s t)))
                   => Sing (t :: PacketType) -> ConvertibleProof d s

convertibleProof :: Sing (d :: PacketDirection)
                 -> Sing (s :: ConnectionState)
                 -> Sing (t :: PacketType) -> Maybe (ConvertibleProof d s)
convertibleProof sd ss st = $(proofCases [|sd|] [|ss|] [|st|]
                              [|Just $ ConvertibleProof st|]
                              [|Nothing|])
                                   
toWire :: ConvertUnused (PacketArgs d s t) (WirePacketArgs d s t)
       => KnxPacket d s t -> WirePacketArgs d s t
toWire (KnxPacket _ _ _ args) = convertUnused args

fromWire :: ConvertUnused (WirePacketArgs d s t) (PacketArgs d s t)
         => Sing d -> Sing s -> Sing t -> WirePacketArgs d s t -> KnxPacket d s t
fromWire sd ss st = KnxPacket sd ss st . convertUnused

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
      SomeSing st -> case convertibleProof sd ss st :: Maybe (ConvertibleProof d s) of
        Just (ConvertibleProof (st' :: Sing t')) -> do
          packet :: KnxPacket d s t' <- fromWire sd ss st' <$> getWire
          return $ WireKnxPacket packet
        Nothing -> fail "Received a packet I don't have a description for. Internal error."
