module KNXd.Client.Internal.PacketArgs where

import Data.ByteString (ByteString)
import Data.HList
import Data.Void
import Data.Word
import KNXd.Client.Internal.Types
       
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