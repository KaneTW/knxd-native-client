{-# LANGUAGE DefaultSignatures, TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}
module KNXd.Client.Internal where

import Data.Bits
import Data.ByteString (ByteString)
import Data.HList
import Data.Serialize
import Data.Singletons
import Data.Singletons.TH
import Data.Void
import Data.Word
import Text.Printf
import KNXd.Client.Internal.TH
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
    

-- |The possible packet types. TODO: find a better way to comment
type family PacketArgs (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType)  where
  PacketArgs 'ToServer s 'ResetConnection = HList '[]
  PacketArgs 'FromServer 'Fresh 'ResetConnection = HList '[]
  
  PacketArgs 'FromServer 'Broken 'InvalidRequest = HList '[]
  PacketArgs 'FromServer s 'ConnectionInuse = HList '[]
  PacketArgs 'FromServer s 'ProcessingError = HList '[]
  PacketArgs 'FromServer s 'ErrorAddrExists = HList '[]
  PacketArgs 'FromServer s 'ErrorMoreDevice = HList '[]
  PacketArgs 'FromServer s 'ErrorTimeout = HList '[]
  PacketArgs 'FromServer s 'ErrorVerify = HList '[]
  
  PacketArgs 'ToServer 'Fresh 'OpenBusmonitor = HList '[]
  PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitor = HList '[]
  PacketArgs 'ToServer 'Fresh 'OpenBusmonitorText = HList '[]
  PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitorText = HList '[]
  PacketArgs 'ToServer 'Fresh 'OpenBusmonitorTs = HList '[]
  PacketArgs 'FromServer 'BusmonitorTs 'OpenBusmonitorTs = HList '[Word32]
  
  PacketArgs 'ToServer 'Fresh 'OpenVbusmonitor = HList '[]
  PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitor = HList '[]
  PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorText = HList '[]
  PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitorText = HList '[]
  PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorTs = HList '[]
  PacketArgs 'FromServer 'BusmonitorTs 'OpenVbusmonitorTs = HList '[Word32]

-- i think i can fold these into one PacketType.
  -- |Destination address
  PacketArgs 'ToServer   'Fresh 'OpenTConnection = HList '[IndividualAddress]
  PacketArgs 'FromServer 'Connection 'OpenTConnection = HList '[]
  -- |Destination address, write-only?
  PacketArgs 'ToServer   'Fresh 'OpenTIndividual = HList '[IndividualAddress, Bool]
  PacketArgs 'FromServer 'Individual 'OpenTIndividual = HList '[]
  -- |Destination  address, write-only?
  PacketArgs 'ToServer   'Fresh 'OpenTGroup = HList '[GroupAddress, Bool]
  PacketArgs 'FromServer 'Group 'OpenTGroup = HList '[]
  -- |write-only?
  PacketArgs 'ToServer   'Fresh 'OpenTBroadcast = HList '[Bool]
  PacketArgs 'FromServer 'Broadcast 'OpenTBroadcast = HList '[]
  -- |Source address
  PacketArgs 'ToServer   'Fresh 'OpenTTpdu = HList '[IndividualAddress]
  PacketArgs 'FromServer 'Tpdu 'OpenTTpdu = HList '[]
  -- |write-only?
  PacketArgs 'ToServer   'Fresh 'OpenGroupcon = HList '[Bool]
  PacketArgs 'FromServer 'GroupSocket 'OpenGroupcon = HList '[]

  PacketArgs 'ToServer 'Fresh 'McConnection = HList '[]
  PacketArgs 'FromServer 'ManagementConnection 'McConnection = HList '[]
  
  PacketArgs 'ToServer 'Fresh 'McIndividual = HList '[IndividualAddress]
  PacketArgs 'FromServer 'ConnectionlessManagementConnection 'McIndividual = HList '[]

  -- |List devices in programming mode
  PacketArgs 'ToServer 'Fresh 'MIndividualAddressRead = HList '[]
  PacketArgs 'FromServer 'Fresh 'MIndividualAddressRead = HList '[[IndividualAddress]]

  -- |Set programming mode of a device
  PacketArgs 'ToServer 'Fresh 'ProgMode = HList '[IndividualAddress, ProgCommand]
  -- |Technically, a state is only returned if ProgCommand ProgStatus is sent
  -- but I'm not adding another type index.
  PacketArgs 'FromServer 'Fresh 'ProgMode = HList '[Maybe Bool]

  -- |Write a new address to a device in programming mode.
  -- Requires exactly one device in programming mode.
  PacketArgs 'ToServer 'Fresh 'MIndividualAddressWrite = HList '[IndividualAddress]
  PacketArgs 'FromServer 'Fresh 'MIndividualAddressWrite = HList '[]

  -- |Read the mask version of the specified EIB device
  PacketArgs 'ToServer 'Fresh 'MaskVersion = HList '[IndividualAddress]
  PacketArgs 'FromServer 'Fresh 'MaskVersion = HList '[Word16]
  
  -- this is a whole can of worms. i'll implement it later, if ever
  --PacketArgs 'ToServer 'Fresh 'LoadImage = Void
  
  -- |Enable the group cache, if possible.
  PacketArgs d 'Fresh 'CacheEnable = HList '[]
  -- |Disable and clear the group cache
  PacketArgs d 'Fresh 'CacheDisable = HList '[]
  -- |Clear the group cache entirely
  PacketArgs d 'Fresh 'CacheClear = HList '[]
  -- |Clear the group cache for the specified group address
  PacketArgs 'ToServer 'Fresh 'CacheRemove = HList '[GroupAddress]

  -- |Reuqest the last group telegram sent from the specified address
  -- with the specified maximum age.
  -- * If an entry is found and age is 0, return the entry
  -- * If an entry is found and age is non-zero, return the entry
  --   only if it's younger than age in seconds
  -- * If no entry is found, it sends a EIB request and
  --   waits for a suitable packet to arrive for about one second.
  --   If nothing is found, return an empty APDU
  --   and cache the group address as not present
  -- Future calls will only cause a new EIB request if the cache is cleared
  -- for this address or a non-zero age was specified and the entry is older than that
  PacketArgs 'ToServer 'Fresh 'CacheRead = HList '[GroupAddress, Word16]
  -- |source, destination, APDU
  PacketArgs 'FromServer 'Fresh 'CacheRead = HList '[GroupAddress, GroupAddress, ByteString]
  
  -- |Request the last group telegram sent from the specified address
  PacketArgs 'ToServer 'Fresh 'CacheReadNowait = HList '[GroupAddress]
  PacketArgs 'FromServer 'Fresh 'CacheReadNowait = HList '[GroupAddress, GroupAddress, ByteString]
  
  -- |TODO. See BCUSDK docs in the meantime.
  PacketArgs 'ToServer 'Fresh 'CacheLastUpdates = HList '[Word16, Word8]
  PacketArgs 'FromServer 'Fresh 'CacheLastUpdates = HList '[Word16, [GroupAddress]]
  
  {-
  PacketArgs 'FromServer 'McKeyWrite = HList '[]
  PacketArgs 'FromServer 'McRestart = HList '[]
  PacketArgs 'FromServer 'McWrite = HList '[]
  PacketArgs 'FromServer 'McWriteNoverify = HList '[]-}

  -- |Unless we explicitly have a type here, forbid it.
  PacketArgs d s t = Void


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

-- i really hope to find a better solution because this case statement is HUGE
convertibleProof :: Sing (d :: PacketDirection)
                 -> Sing (s :: ConnectionState)
                 -> Sing (t :: PacketType) -> ConvertibleProof d s
convertibleProof sd ss st = $(proofCases [|sd|] [|ss|] [|st|] [|ConvertibleProof st|])
                                   
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
      SomeSing st -> case convertibleProof sd ss st :: ConvertibleProof d s of
        ConvertibleProof (st' :: Sing t') -> do
          packet :: KnxPacket d s t' <- fromWire sd ss st' <$> getWire
          return $ WireKnxPacket packet
