module KNXd.Client.Internal.PacketArgs where

import Data.ByteString (ByteString)
import Data.HList
import Data.Singletons
import Data.Word
import KNXd.Client.Internal.Serialize
import KNXd.Client.Internal.Types

-- |A KNX packet. Arguments are determined by it's type, see 'PacketArgs'
data KnxPacket (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType) where
  KnxPacket :: Sing d -> Sing s -> Sing t -> PacketArgs d s t -> KnxPacket d s t

deriving instance PacketC d s t => Show (KnxPacket d s t)
deriving instance PacketC d s t => Eq (KnxPacket d s t)

--TODO: maybe it might be better to split this into two classes?
-- |Used to convert to/from wire representation (fill in/remove any Unused vaues)
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

instance (ConvertWire a, DefaultValue a) => ConvertWire (Unused a) where
  putWire Unused = putWire (defaultValue :: a)
  getWire = return Unused


-- |Class and associated type family for valid packet types
class (Eq (PacketArgs d s t), Show (PacketArgs d s t)
      , ConvertWire (WirePacketArgs d s t))
  => PacketC (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType) where
  type PacketArgs d s t :: *
  type WirePacketArgs d s t :: *
  type instance WirePacketArgs d s t = PacketArgs d s t

  -- |Prepare a KnxPacket for sending
  toWire :: KnxPacket d s t -> WirePacketArgs d s t

  default toWire :: WirePacketArgs d s t ~ PacketArgs d s t
                 => KnxPacket d s t -> PacketArgs d s t
  toWire (KnxPacket _ _ _ args) = args

  -- |Convert a received packet to an actual packet
  fromWire :: Sing d -> Sing s  -> Sing t -> WirePacketArgs d s t -> KnxPacket d s t

  default fromWire :: (PacketArgs d s t ~ WirePacketArgs d s t)
                   => Sing d -> Sing s -> Sing t -> PacketArgs d s t -> KnxPacket d s t
  fromWire = KnxPacket
  

instance PacketC 'ToServer s 'ResetConnection where 
  type PacketArgs 'ToServer s 'ResetConnection = HList '[]
instance PacketC 'FromServer 'Fresh 'ResetConnection where 
  type PacketArgs 'FromServer 'Fresh 'ResetConnection = HList '[]
  
instance PacketC 'FromServer 'Broken 'InvalidRequest where 
  type PacketArgs 'FromServer 'Broken 'InvalidRequest = HList '[]

instance PacketC 'FromServer s 'ConnectionInuse where 
  type PacketArgs 'FromServer s 'ConnectionInuse = HList '[]

instance PacketC 'FromServer s 'ProcessingError where 
  type PacketArgs 'FromServer s 'ProcessingError = HList '[]
  
-- |Technically these can only follow MIndividualAddressWrite. But adding another state is meh
instance PacketC 'FromServer 'Fresh 'ErrorAddrExists where 
  type PacketArgs 'FromServer 'Fresh 'ErrorAddrExists = HList '[]

instance PacketC 'FromServer 'Fresh 'ErrorMoreDevice where 
  type PacketArgs 'FromServer 'Fresh 'ErrorMoreDevice = HList '[]

instance PacketC 'FromServer 'Fresh 'ErrorTimeout where 
  type PacketArgs 'FromServer 'Fresh 'ErrorTimeout = HList '[]

instance PacketC 'FromServer 'Fresh 'ErrorVerify where 
  type PacketArgs 'FromServer 'Fresh 'ErrorVerify = HList '[]

-- |Busmonitor stuff
instance PacketC 'ToServer 'Fresh 'OpenBusmonitor where 
  type PacketArgs 'ToServer 'Fresh 'OpenBusmonitor = HList '[]
instance PacketC 'FromServer 'Busmonitor 'OpenBusmonitor where 
  type PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitor = HList '[]

instance PacketC 'ToServer 'Fresh 'OpenBusmonitorText where 
  type PacketArgs 'ToServer 'Fresh 'OpenBusmonitorText = HList '[]
instance PacketC 'FromServer 'Busmonitor 'OpenBusmonitorText where 
  type PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitorText = HList '[]

instance PacketC 'ToServer 'Fresh 'OpenBusmonitorTs where 
  type PacketArgs 'ToServer 'Fresh 'OpenBusmonitorTs = HList '[]
instance PacketC 'FromServer 'BusmonitorTs 'OpenBusmonitorTs where 
  type PacketArgs 'FromServer 'BusmonitorTs 'OpenBusmonitorTs = HList '[Word32]
  
instance PacketC 'ToServer 'Fresh 'OpenVbusmonitor where 
  type PacketArgs 'ToServer 'Fresh 'OpenVbusmonitor = HList '[]
instance PacketC 'FromServer 'Busmonitor 'OpenVbusmonitor where 
  type PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitor = HList '[]

instance PacketC 'ToServer 'Fresh 'OpenVbusmonitorText where 
  type PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorText = HList '[]
instance PacketC 'FromServer 'Busmonitor 'OpenVbusmonitorText where 
  type PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitorText = HList '[]

instance PacketC 'ToServer 'Fresh 'OpenVbusmonitorTs where 
  type PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorTs = HList '[]
instance PacketC 'FromServer 'BusmonitorTs 'OpenVbusmonitorTs where 
  type PacketArgs 'FromServer 'BusmonitorTs 'OpenVbusmonitorTs = HList '[Word32]

-- |Just the packet data
instance PacketC 'FromServer 'Busmonitor 'BusmonitorPacket where
  type PacketArgs 'FromServer 'Busmonitor 'BusmonitorPacket = HList '[ByteString]

-- |Busmonitor status field, timestamp, packet data
instance PacketC 'FromServer 'Busmonitor 'BusmonitorPacket where
  type PacketArgs 'FromServer 'Busmonitor 'BusmonitorPacket = HList '[Word8, Word32, ByteString]

-- |Destination address
instance PacketC 'ToServer 'Fresh 'OpenTConnection where 
  type PacketArgs 'ToServer 'Fresh 'OpenTConnection = HList '[IndividualAddress]
  type WirePacketArgs 'ToServer 'Fresh 'OpenTConnection = HList '[IndividualAddress, Unused Bool]
  
  toWire (KnxPacket _ _ _ args) = convertUnused args
  fromWire ss sd st = KnxPacket ss sd st . convertUnused
  
instance PacketC 'FromServer 'Connection 'OpenTConnection where 
  type PacketArgs 'FromServer 'Connection 'OpenTConnection = HList '[]

instance PacketC 'ToServer 'Connection 'ApduPacket where
  type PacketArgs 'ToServer 'Connection 'ApduPacket = HList '[APDU]
instance PacketC 'FromServer 'Connection 'ApduPacket where
  type PacketArgs 'FromServer 'Connection 'ApduPacket = HList '[APDU]  

-- |Destination address, write-only?
instance PacketC 'ToServer   'Fresh 'OpenTIndividual where 
  type PacketArgs 'ToServer   'Fresh 'OpenTIndividual = HList '[IndividualAddress, Bool]
instance PacketC 'FromServer 'Individual 'OpenTIndividual where 
  type PacketArgs 'FromServer 'Individual 'OpenTIndividual = HList '[]

instance PacketC 'ToServer 'Individual 'ApduPacket where
  type PacketArgs 'ToServer 'Individual 'ApduPacket = HList '[APDU]
instance PacketC 'FromServer 'Individual 'ApduPacket where
  type PacketArgs 'FromServer 'Individual 'ApduPacket = HList '[APDU]  

-- |Destination address, write-only?
instance PacketC 'ToServer 'Fresh 'OpenTGroup where 
  type PacketArgs 'ToServer 'Fresh 'OpenTGroup = HList '[GroupAddress, Bool]
instance PacketC 'FromServer 'Group 'OpenTGroup where 
  type PacketArgs 'FromServer 'Group 'OpenTGroup = HList '[]

instance PacketC 'ToServer 'Group 'ApduPacket where
  type PacketArgs 'ToServer 'Group 'ApduPacket = HList '[APDU]
instance PacketC 'FromServer 'Group 'ApduPacket where
  type PacketArgs 'FromServer 'Group 'ApduPacket = HList '[GroupAddress, APDU]


-- |write-only?
instance PacketC 'ToServer 'Fresh 'OpenTBroadcast where 
  type PacketArgs 'ToServer 'Fresh 'OpenTBroadcast = HList '[Bool]
  type WirePacketArgs 'ToServer 'Fresh 'OpenTBroadcast = HList '[Unused Word16, Bool]
  
  toWire (KnxPacket _ _ _ args) = convertUnused args
  fromWire ss sd st = KnxPacket ss sd st . convertUnused

instance PacketC 'FromServer 'Broadcast 'OpenTBroadcast where 
  type PacketArgs 'FromServer 'Broadcast 'OpenTBroadcast = HList '[]

instance PacketC 'ToServer 'Broadcast 'ApduPacket where
  type PacketArgs 'ToServer 'Broadcast 'ApduPacket = HList '[APDU]
instance PacketC 'FromServer 'Broadcast 'ApduPacket where
  type PacketArgs 'FromServer 'Broadcast 'ApduPacket = HList '[IndividualAddress, APDU]

-- |Source address
instance PacketC 'ToServer 'Fresh 'OpenTTpdu where 
  type PacketArgs 'ToServer 'Fresh 'OpenTTpdu = HList '[IndividualAddress]
  type WirePacketArgs 'ToServer 'Fresh 'OpenTTpdu = HList '[IndividualAddress, Unused Bool]

  toWire (KnxPacket _ _ _ args) = convertUnused args
  fromWire ss sd st = KnxPacket ss sd st . convertUnused

instance PacketC 'FromServer 'Tpdu 'OpenTTpdu where 
  type PacketArgs 'FromServer 'Tpdu 'OpenTTpdu = HList '[]

-- |I'm not sure whether a TPDU returns an APDU.
instance PacketC 'ToServer 'Tpdu 'ApduPacket where
  type PacketArgs 'ToServer 'Tpdu 'ApduPacket = HList '[IndividualAddress, APDU]
instance PacketC 'FromServer 'Tpdu 'ApduPacket where
  type PacketArgs 'FromServer 'Tpdu 'ApduPacket = HList '[IndividualAddress, APDU]

-- |write-only?
instance PacketC 'ToServer 'Fresh 'OpenGroupcon where 
  type PacketArgs 'ToServer 'Fresh 'OpenGroupcon = HList '[Bool]
  type WirePacketArgs 'ToServer 'Fresh 'OpenGroupcon = HList '[Unused Word16, Bool]

  toWire (KnxPacket _ _ _ args) = convertUnused args
  fromWire ss sd st = KnxPacket ss sd st . convertUnused

instance PacketC 'FromServer 'GroupSocket 'OpenGroupcon where 
  type PacketArgs 'FromServer 'GroupSocket 'OpenGroupcon = HList '[]

-- |Destination
instance PacketC 'ToServer 'GroupSocket 'GroupPacket where
  type PacketArgs 'ToServer 'GroupSocket 'GroupPacket = HList '[GroupAddress, APDU]
-- |Source, destination
instance PacketC 'FromServer 'GroupSocket 'GroupPacket where
  type PacketArgs 'FromServer 'GroupSocket 'GroupPacket = HList '[GroupAddress, GroupAddress, APDU]

instance PacketC 'ToServer 'Fresh 'McConnection where 
  type PacketArgs 'ToServer 'Fresh 'McConnection = HList '[]
instance PacketC 'FromServer 'ManagementConnection 'McConnection where 
  type PacketArgs 'FromServer 'ManagementConnection 'McConnection = HList '[]
  
instance PacketC 'ToServer 'Fresh 'McIndividual where 
  type PacketArgs 'ToServer 'Fresh 'McIndividual = HList '[IndividualAddress]
instance PacketC 'FromServer 'ConnectionlessManagementConnection 'McIndividual where 
  type PacketArgs 'FromServer 'ConnectionlessManagementConnection 'McIndividual = HList '[]

-- |List devices in programming mode
instance PacketC 'ToServer 'Fresh 'MIndividualAddressRead where 
  type PacketArgs 'ToServer 'Fresh 'MIndividualAddressRead = HList '[]
instance PacketC 'FromServer 'Fresh 'MIndividualAddressRead where 
  type PacketArgs 'FromServer 'Fresh 'MIndividualAddressRead = HList '[[IndividualAddress]]

-- |Set programming mode of a device
instance PacketC 'ToServer 'Fresh 'ProgMode where 
  type PacketArgs 'ToServer 'Fresh 'ProgMode = HList '[IndividualAddress, ProgCommand]
-- |A state is only returned if ProgCommand ProgStatus is sent
instance PacketC 'FromServer 'Fresh 'ProgMode where 
  type PacketArgs 'FromServer 'Fresh 'ProgMode = HList '[Maybe Bool]

-- |Write a new address to a device in programming mode.
-- Requires exactly one device in programming mode.
instance PacketC 'ToServer 'Fresh 'MIndividualAddressWrite where 
  type PacketArgs 'ToServer 'Fresh 'MIndividualAddressWrite = HList '[IndividualAddress]
instance PacketC 'FromServer 'Fresh 'MIndividualAddressWrite where 
  type PacketArgs 'FromServer 'Fresh 'MIndividualAddressWrite = HList '[]

-- |Read the mask version of the specified EIB device
instance PacketC 'ToServer 'Fresh 'MaskVersion where 
  type PacketArgs 'ToServer 'Fresh 'MaskVersion = HList '[IndividualAddress]
instance PacketC 'FromServer 'Fresh 'MaskVersion where 
  type PacketArgs 'FromServer 'Fresh 'MaskVersion = HList '[Word16]
  
-- this is a whole can of worms. i'll implement it later, if ever
--'ToServer 'Fresh 'LoadImage = Void
  
-- |Enable the group cache, if possible.
instance PacketC d 'Fresh 'CacheEnable where 
  type PacketArgs d 'Fresh 'CacheEnable = HList '[]
-- |Disable and clear the group cache
instance PacketC d 'Fresh 'CacheDisable where 
  type PacketArgs d 'Fresh 'CacheDisable = HList '[]
-- |Clear the group cache entirely
instance PacketC d 'Fresh 'CacheClear where 
  type PacketArgs d 'Fresh 'CacheClear = HList '[]
-- |Clear the group cache for the specified group address
instance PacketC 'ToServer 'Fresh 'CacheRemove where 
  type PacketArgs 'ToServer 'Fresh 'CacheRemove = HList '[GroupAddress]

-- |Request the last group telegram sent from the specified address
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
instance PacketC 'ToServer 'Fresh 'CacheRead where 
  type PacketArgs 'ToServer 'Fresh 'CacheRead = HList '[GroupAddress, Word16]
instance PacketC 'FromServer 'Fresh 'CacheRead where
  -- |source, destination, APDU
  type PacketArgs 'FromServer 'Fresh 'CacheRead = HList '[GroupAddress, GroupAddress, ByteString]
  
-- |Request the last group telegram sent from the specified address
instance PacketC 'ToServer 'Fresh 'CacheReadNowait where 
  type PacketArgs 'ToServer 'Fresh 'CacheReadNowait = HList '[GroupAddress]
instance PacketC 'FromServer 'Fresh 'CacheReadNowait where 
  type PacketArgs 'FromServer 'Fresh 'CacheReadNowait = HList '[GroupAddress, GroupAddress, ByteString]
  
-- |TODO. See BCUSDK docs in the meantime.
instance PacketC 'ToServer 'Fresh 'CacheLastUpdates where 
  type PacketArgs 'ToServer 'Fresh 'CacheLastUpdates = HList '[Word16, Word8]
instance PacketC 'FromServer 'Fresh 'CacheLastUpdates where 
  type PacketArgs 'FromServer 'Fresh 'CacheLastUpdates = HList '[Word16, [GroupAddress]]
