{-# LANGUAGE DefaultSignatures, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module KNXd.Client.Internal where

import Data.Bits
import Data.ByteString (ByteString)
import Data.HList
import Data.Serialize
import Data.Singletons
import Data.Singletons.TH
import Data.Word
import Text.Printf
import KNXd.Client.Internal.Types

splitAddress :: Word16 -> (Word16, Word16, Word16)
splitAddress addr = let h = shift addr (-12)
                        m = shift (addr .&. 0x0f00) (-8)
                        l = addr .&. 0xff
                    in (h,m,l)

combineAddress :: Word16 -> Word16 -> Word16 -> Word16
combineAddress h m l = shift (h .&. 0xf) 12 .|. shift (m .&. 0xf) 8 .|. (l .&. 0xff)

-- |An individual, physical address (e.g. 0.0.1)
newtype IndividualAddress = IndividualAddress Word16
                          deriving (Eq, Ord, ConvertWire)

instance Show IndividualAddress where
  show (IndividualAddress addr) = let (h,m,l) = splitAddress addr
                                  in printf "%d.%d.%d" h m l

-- |A group address (e.g. 1/2/13)
newtype GroupAddress = GroupAddress Word16
                     deriving (Eq, Ord, ConvertWire)

instance Show GroupAddress where
  show (GroupAddress addr) = let (h,m,l) = splitAddress addr
                             in printf "%d/%d/%d" h m l

-- |A KNX packet. Arguments are determined by it's type, see 'PacketArgs'
data KnxPacket (d :: PacketDirection) (t :: PacketType) where
  KnxPacket :: (SingI d, SingI t) => PacketArgs d t -> KnxPacket d t

-- |When receiving a packet, we don't know what PacketType it's indexed by.
-- But we do know where it came from.
data WireKnxPacket d where
  WireKnxPacket :: (ConvertWire (WirePacketArgs d t)
                 , (ConvertUnused (PacketArgs d t) (WirePacketArgs d t))
                 , (ConvertUnused (WirePacketArgs d t) (PacketArgs d t)))
                => KnxPacket d t -> WireKnxPacket d

-- |Type -> Sing
getTypeSing ::  KnxPacket d t -> Sing t 
getTypeSing (KnxPacket _) = sing

getDirSing :: KnxPacket d t -> Sing d
getDirSing (KnxPacket _) = sing

-- |Type -> Term
getPacketType :: KnxPacket d t -> PacketType
getPacketType = fromSing . getTypeSing

getPacketDirection :: KnxPacket d t -> PacketDirection
getPacketDirection = fromSing . getDirSing
    

-- |The possible packet types. TODO: find a better way to comment
type family PacketArgs (d :: PacketDirection) (t :: PacketType)  where
  -- |Tick length in ns
  PacketArgs 'FromServer 'OpenBusmonitorTs = HList '[Word32]
  PacketArgs 'FromServer 'OpenVbusmonitorTs = HList '[Word32]
  
  -- |Destination address
  PacketArgs 'ToServer 'OpenTConnection = HList '[IndividualAddress]
  
  -- |Destination address, write-only?
  PacketArgs 'ToServer 'OpenTIndividual = HList '[IndividualAddress, Bool]

  -- |Destination address, write-only?
  PacketArgs 'ToServer 'OpenTGroup = HList '[GroupAddress, Bool]
  
  -- |write-only?
  PacketArgs 'ToServer 'OpenTBroadcast = HList '[Bool]
  
  -- |Source address
  PacketArgs 'ToServer 'OpenTTpdu = HList '[IndividualAddress]

  -- |write-only?
  PacketArgs 'ToServer 'OpenGroupcon = HList '[Bool]

  -- and more...

  -- |Most cases are nullary, so fall back onto that
  PacketArgs d t = HList '[]


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
type family WirePacketArgs (d :: PacketDirection) (t :: PacketType) where
  WirePacketArgs 'ToServer 'OpenTConnection = HList '[IndividualAddress, Unused Bool]
  WirePacketArgs 'ToServer 'OpenTIndividual = HList '[IndividualAddress, Bool]
  WirePacketArgs 'ToServer 'OpenTGroup = HList '[GroupAddress, Bool]
  WirePacketArgs 'ToServer 'OpenTBroadcast = HList '[Unused Word16, Bool]
  WirePacketArgs 'ToServer 'OpenTTpdu = HList '[IndividualAddress, Unused Bool]
  WirePacketArgs 'ToServer 'OpenGroupcon = HList '[Unused Word16, Bool]
  -- and more...
  
  WirePacketArgs d t = PacketArgs d t

-- |Used to convert to/from wire representation
class ConvertUnused k l where
  convertUnused :: k -> l

instance ConvertUnused (HList '[]) (HList '[]) where
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


-- |Some types need nonstandard serialization, 
-- so it's easier to just serialize in a special class.
class ConvertWire a where
  putWire :: Putter a
  default putWire :: Serialize a => Putter a
  putWire = put

  getWire :: Get a
  default getWire :: Serialize a => Get a
  getWire = get

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

instance ConvertWire (HList '[]) where
  putWire _ = return ()
  getWire   = return HNil

instance (ConvertWire e, ConvertWire (HList l))
         => ConvertWire (HList (e ': l)) where
  putWire (HCons e l) = putWire e >> putWire l
  getWire = HCons <$> getWire <*> getWire

instance (ConvertWire a, DefaultValue a) => ConvertWire (Unused a) where
  putWire Unused = putWire (defaultValue :: a)
  getWire = return Unused


data SerializeEvidence d where
  SerializeEvidence :: (ConvertWire (WirePacketArgs d t)
                     , (ConvertUnused (WirePacketArgs d t) (PacketArgs d t))
                     , (ConvertUnused (PacketArgs d t) (WirePacketArgs d t)))
                    => Sing t -> SerializeEvidence d

--todo: not all packets can be sent or received
-- big hack. if someone has a better solution PLEASE tell me
serializeEvidence :: Sing (d :: PacketDirection) -> Sing (t :: PacketType) -> SerializeEvidence d
serializeEvidence sd st = case sd of
  -- bug? with singletons requiring duplicate splices
  SToServer   -> $(sCases ''PacketType [|st|] [|SerializeEvidence st|])
  SFromServer -> $(sCases ''PacketType [|st|] [|SerializeEvidence st|])
                                   
toWire :: ConvertUnused (PacketArgs d t) (WirePacketArgs d t)
       => KnxPacket d t -> WirePacketArgs d t
toWire (KnxPacket args) = convertUnused args


fromWire :: (ConvertUnused (WirePacketArgs d t) (PacketArgs d t), SingI t, SingI d)
         => WirePacketArgs d t -> KnxPacket d t
fromWire = KnxPacket . convertUnused

--todo: sigh. this doesn't actually work, i think
{-
Consider
```
case (r :: WireKnxPacket d) of
  WireKnxPacket p -> <do stuff with p>
```
then we need to handle everything related to the packet type in that case block.
-}
instance SingI d => Serialize (WireKnxPacket d) where
  put (WireKnxPacket packet) = do
    putNested (putWord16be . fromIntegral) $ do
      putWire . fromPacketType . getPacketType $ packet
      putWire $ toWire packet
    
  get = getNested (fromIntegral <$> getWord16be) $ do
    --todo: instance of Serialize for packetType to avoid error
    t <- toPacketType <$> getWord16be
    let sd = sing :: Sing d
    case toSing t of
      SomeSing st -> case serializeEvidence sd st of
        SerializeEvidence (st' :: Sing t') -> withSingI st' $ do
          packet :: KnxPacket d t' <- fromWire <$> getWire
          return $ WireKnxPacket packet
