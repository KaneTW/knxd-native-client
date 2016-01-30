module KNXd.Client.Internal.PacketArgs where

import Data.ByteString (ByteString)
import Data.HList
import Data.Word
import KNXd.Client.Internal.Types
       
-- |The possible packet types. TODO: find a better way to comment
type family PacketArgs (d :: PacketDirection) (s :: ConnectionState) (t :: PacketType)

type instance PacketArgs 'ToServer s 'ResetConnection = HList '[]
type instance PacketArgs 'FromServer 'Fresh 'ResetConnection = HList '[]
  
type instance PacketArgs 'FromServer 'Broken 'InvalidRequest = HList '[]
type instance PacketArgs 'FromServer s 'ConnectionInuse = HList '[]
type instance PacketArgs 'FromServer s 'ProcessingError = HList '[]
type instance PacketArgs 'FromServer s 'ErrorAddrExists = HList '[]
type instance PacketArgs 'FromServer s 'ErrorMoreDevice = HList '[]
type instance PacketArgs 'FromServer s 'ErrorTimeout = HList '[]
type instance PacketArgs 'FromServer s 'ErrorVerify = HList '[]
  
type instance PacketArgs 'ToServer 'Fresh 'OpenBusmonitor = HList '[]
type instance PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitor = HList '[]
type instance PacketArgs 'ToServer 'Fresh 'OpenBusmonitorText = HList '[]
type instance PacketArgs 'FromServer 'Busmonitor 'OpenBusmonitorText = HList '[]
type instance PacketArgs 'ToServer 'Fresh 'OpenBusmonitorTs = HList '[]
type instance PacketArgs 'FromServer 'BusmonitorTs 'OpenBusmonitorTs = HList '[Word32]
  
type instance PacketArgs 'ToServer 'Fresh 'OpenVbusmonitor = HList '[]
type instance PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitor = HList '[]
type instance PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorText = HList '[]
type instance PacketArgs 'FromServer 'Busmonitor 'OpenVbusmonitorText = HList '[]
type instance PacketArgs 'ToServer 'Fresh 'OpenVbusmonitorTs = HList '[]
type instance PacketArgs 'FromServer 'BusmonitorTs 'OpenVbusmonitorTs = HList '[Word32]

-- i think i can fold these into one PacketType.
-- |Destination address
type instance PacketArgs 'ToServer   'Fresh 'OpenTConnection = HList '[IndividualAddress]
type instance PacketArgs 'FromServer 'Connection 'OpenTConnection = HList '[]
-- |Destination address, write-only?
type instance PacketArgs 'ToServer   'Fresh 'OpenTIndividual = HList '[IndividualAddress, Bool]
type instance PacketArgs 'FromServer 'Individual 'OpenTIndividual = HList '[]
-- |Destination  address, write-only?
type instance PacketArgs 'ToServer   'Fresh 'OpenTGroup = HList '[GroupAddress, Bool]
type instance PacketArgs 'FromServer 'Group 'OpenTGroup = HList '[]
-- |write-only?
type instance PacketArgs 'ToServer   'Fresh 'OpenTBroadcast = HList '[Bool]
type instance PacketArgs 'FromServer 'Broadcast 'OpenTBroadcast = HList '[]
-- |Source address
type instance PacketArgs 'ToServer   'Fresh 'OpenTTpdu = HList '[IndividualAddress]
type instance PacketArgs 'FromServer 'Tpdu 'OpenTTpdu = HList '[]
-- |write-only?
type instance PacketArgs 'ToServer   'Fresh 'OpenGroupcon = HList '[Bool]
type instance PacketArgs 'FromServer 'GroupSocket 'OpenGroupcon = HList '[]

type instance PacketArgs 'ToServer 'Fresh 'McConnection = HList '[]
type instance PacketArgs 'FromServer 'ManagementConnection 'McConnection = HList '[]
  
type instance PacketArgs 'ToServer 'Fresh 'McIndividual = HList '[IndividualAddress]
type instance PacketArgs 'FromServer 'ConnectionlessManagementConnection 'McIndividual = HList '[]

-- |List devices in programming mode
type instance PacketArgs 'ToServer 'Fresh 'MIndividualAddressRead = HList '[]
type instance PacketArgs 'FromServer 'Fresh 'MIndividualAddressRead = HList '[[IndividualAddress]]

-- |Set programming mode of a device
type instance PacketArgs 'ToServer 'Fresh 'ProgMode = HList '[IndividualAddress, ProgCommand]
-- |Technically, a state is only returned if ProgCommand ProgStatus is sent
-- but I'm not adding another type index.
type instance PacketArgs 'FromServer 'Fresh 'ProgMode = HList '[Maybe Bool]

-- |Write a new address to a device in programming mode.
-- Requires exactly one device in programming mode.
type instance PacketArgs 'ToServer 'Fresh 'MIndividualAddressWrite = HList '[IndividualAddress]
type instance PacketArgs 'FromServer 'Fresh 'MIndividualAddressWrite = HList '[]

-- |Read the mask version of the specified EIB device
type instance PacketArgs 'ToServer 'Fresh 'MaskVersion = HList '[IndividualAddress]
type instance PacketArgs 'FromServer 'Fresh 'MaskVersion = HList '[Word16]
  
-- this is a whole can of worms. i'll implement it later, if ever
--PacketArgs 'ToServer 'Fresh 'LoadImage = Void
  
-- |Enable the group cache, if possible.
type instance PacketArgs d 'Fresh 'CacheEnable = HList '[]
-- |Disable and clear the group cache
type instance PacketArgs d 'Fresh 'CacheDisable = HList '[]
-- |Clear the group cache entirely
type instance PacketArgs d 'Fresh 'CacheClear = HList '[]
-- |Clear the group cache for the specified group address
type instance PacketArgs 'ToServer 'Fresh 'CacheRemove = HList '[GroupAddress]

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
type instance PacketArgs 'ToServer 'Fresh 'CacheRead = HList '[GroupAddress, Word16]
-- |source, destination, APDU
type instance PacketArgs 'FromServer 'Fresh 'CacheRead = HList '[GroupAddress, GroupAddress, ByteString]
  
-- |Request the last group telegram sent from the specified address
type instance PacketArgs 'ToServer 'Fresh 'CacheReadNowait = HList '[GroupAddress]
type instance PacketArgs 'FromServer 'Fresh 'CacheReadNowait = HList '[GroupAddress, GroupAddress, ByteString]
  
-- |TODO. See BCUSDK docs in the meantime.
type instance PacketArgs 'ToServer 'Fresh 'CacheLastUpdates = HList '[Word16, Word8]
type instance PacketArgs 'FromServer 'Fresh 'CacheLastUpdates = HList '[Word16, [GroupAddress]]

