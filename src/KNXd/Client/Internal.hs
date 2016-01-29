{-# LANGUAGE DefaultSignatures, RankNTypes #-}
module KNXd.Client.Internal where

import Data.Bits
import Data.ByteString (ByteString)
import Data.HList
import Data.Serialize
import Data.Singletons
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
newtype IndividualAddress = IndividualAddress Word16 deriving (Eq, Ord)
instance Show IndividualAddress where
  show (IndividualAddress addr) = let (h,m,l) = splitAddress addr
                                  in printf "%d.%d.%d" h m l

-- |A group address (e.g. 1/2/13)
newtype GroupAddress = GroupAddress Word16 deriving (Eq, Ord)
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
                 , (ConvertUnused (PacketArgs d t) (WirePacketArgs d t)))
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
  PacketArgs d 'ResetConnection = HList '[]
  PacketArgs 'FromServer 'InvalidRequest = HList '[]
  PacketArgs 'FromServer 'ConnectionInuse = HList '[]
  PacketArgs 'FromServer 'ProcessingError = HList '[]
  
  PacketArgs d 'OpenBusmonitor = HList '[]
  PacketArgs d 'OpenBusmonitorText = HList '[]
  PacketArgs 'ToServer 'OpenBusmonitorTs = HList '[]
  PacketArgs 'FromServer 'OpenBusmonitorTs = HList '[Word32]
  PacketArgs d 'OpenVbusmonitor = HList '[]
  PacketArgs d 'OpenVbusmonitorText = HList '[]
  PacketArgs 'ToServer 'OpenVbusmonitorTs = HList '[]
  PacketArgs 'FromServer 'OpenVbusmonitorTs = HList '[Word32]
  
  PacketArgs 'FromServer 'OpenTConnection = HList '[]
  -- |Destination address
  PacketArgs 'ToServer   'OpenTConnection = HList '[IndividualAddress]
  
  PacketArgs 'FromServer 'OpenTIndividual = HList '[]
  -- |Destination address, write-only?
  PacketArgs 'ToServer   'OpenTIndividual = HList '[IndividualAddress, Bool]

  PacketArgs 'FromServer 'OpenTGroup = HList '[]
  -- |Destination  address, write-only?
  PacketArgs 'ToServer   'OpenTGroup = HList '[GroupAddress, Bool]
  
  PacketArgs 'FromServer 'OpenTBroadcast = HList '[]
  -- |write-only?
  PacketArgs 'ToServer   'OpenTBroadcast = HList '[Bool]
  
  PacketArgs 'FromServer 'OpenTTpdu = HList '[]
  -- |Source address
  PacketArgs 'ToServer   'OpenTTpdu = HList '[IndividualAddress]
  
  PacketArgs 'FromServer 'OpenGroupcon = HList '[]
  -- |write-only?
  PacketArgs 'ToServer   'OpenGroupcon = HList '[Bool]
  
  PacketArgs 'FromServer 'ErrorAddrExists = HList '[]
  PacketArgs 'FromServer 'ErrorMoreDevice = HList '[]
  PacketArgs 'FromServer 'ErrorTimeout = HList '[]
  PacketArgs 'FromServer 'ErrorVerify = HList '[]
  PacketArgs d 'CacheClear = HList '[]
  PacketArgs d 'CacheDisable = HList '[]
  PacketArgs d 'CacheEnable = HList '[]
  PacketArgs 'FromServer 'CacheRemove = HList '[]
  PacketArgs 'FromServer 'McConnection = HList '[]
  PacketArgs 'FromServer 'McIndividual = HList '[]
  PacketArgs 'FromServer 'McKeyWrite = HList '[]
  PacketArgs 'FromServer 'McRestart = HList '[]
  PacketArgs 'FromServer 'McWrite = HList '[]
  PacketArgs 'FromServer 'McWriteNoverify = HList '[]
  PacketArgs 'FromServer 'MIndividualAddressWrite = HList '[]
  -- and more.


-- |Default value for serializing unused fields
class DefaultValue a where
  defaultValue :: a

instance DefaultValue Word16 where
  defaultValue = 0

instance DefaultValue Bool where
  defaultValue = False

-- |Unused fields that need to be present on the wire but don't have any actual semantics
newtype Unused a = Unused a deriving Show

mkUnused :: DefaultValue a => Unused a
mkUnused = Unused defaultValue

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
  convertUnused k = HCons mkUnused $ convertUnused k

instance ConvertUnused (HList k) (HList l)
         => ConvertUnused (HList (e ': k)) (HList (e ': l)) where
  convertUnused (HCons el k) = HCons el $ convertUnused k

instance ConvertUnused (HList k) (HList l)
         => ConvertUnused (HList (Unused el ': k)) (HList l) where
  convertUnused (HCons _ k) = convertUnused k


toWire :: ConvertUnused (PacketArgs d t) (WirePacketArgs d t)
       => KnxPacket d t -> WirePacketArgs d t
toWire (KnxPacket args) = convertUnused args

fromWire :: (ConvertUnused (WirePacketArgs d t) (PacketArgs d t), SingI t, SingI d)
         => WirePacketArgs d t -> KnxPacket d t
fromWire = KnxPacket . convertUnused

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

instance SingI d => Serialize (WireKnxPacket d) where
  put (WireKnxPacket packet) = do
    putNested (putWord16be . fromIntegral) $ do
      putWire . fromPacketType . getPacketType $ packet
      putWire $ toWire packet
    
  get = getNested (fromIntegral <$> getWord16be) $ do
    --todo: instance of Serialize for packetType to avoid error
    t <- toPacketType <$> getWord16be
    case toSing t of
      SomeSing (sb :: Sing (t :: PacketType)) -> do
        packet :: KnxPacket d t <- fromWire <$> getWire
        undefined -- return $ WireKnxPacket packet

