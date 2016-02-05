{-# LANGUAGE TemplateHaskell #-}
module KNXd.Client.Internal.Types where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word
import Data.Singletons.TH
import Text.Printf

-- |APDUs have structure, too. But we're approximating them as ByteStrings for now.
type APDU = ByteString

splitAddress :: Word16 -> (Word16, Word16, Word16)
splitAddress addr = let h = shift addr (-12)
                        m = shift (addr .&. 0x0f00) (-8)
                        l = addr .&. 0xff
                    in (h,m,l)

combineAddress :: Word16 -> Word16 -> Word16 -> Word16
combineAddress h m l = shift (h .&. 0xf) 12 .|. shift (m .&. 0xf) 8 .|. (l .&. 0xff)

-- |An individual, physical address (e.g. @0.0.1@)
newtype IndividualAddress = IndividualAddress Word16
                          deriving (Eq, Ord)

instance Show IndividualAddress where
  show (IndividualAddress addr) = let (h,m,l) = splitAddress addr
                                  in printf "%d.%d.%d" h m l

-- |A group address (e.g. @1\/2\/13@)
newtype GroupAddress = GroupAddress Word16
                     deriving (Eq, Ord)

instance Show GroupAddress where
  show (GroupAddress addr) = let (h,m,l) = splitAddress addr
                             in printf "%d/%d/%d" h m l

data ProgCommand
  = ProgOn
  | ProgOff
  | ProgToggle
  | ProgStatus
  deriving (Show, Eq, Typeable, Enum, Bounded)

data PacketDirection
  = FromServer
  | ToServer
  deriving (Show, Eq, Typeable)
  
data ConnectionState
  -- |A fresh connection, or a freshly resetted one.
  = Fresh
  -- |When a connection is in this state, consider it unsalvagable and discard it.
  | Broken
  -- |Busmonitor mode, either real or virtual. Real prevents sending of frames.
  | Busmonitor
  -- |Includes the busmonitor status field and a timestamp.
  | BusmonitorTs
  -- |The following are Layer 4 modes
  | Broadcast
  -- |Communication using 'GroupAddress' with a specific address
  | Group
  -- |Communication using 'IndividualAddress' with a specific address
  | Individual
  -- |Raw communication with a given device
  | Tpdu
  -- |A proper KNX connection. See BCUSDK docs for details. Take care when using.
  | Connection
  -- |Group communication that is not bound to a specific address.
  -- Can be used to communicate with a whole range of addresses. Read the docs.
  | GroupSocket
  -- |Layer 7 management connection.
  | ManagementConnection
  -- |'Connection'-less management connection.
  -- Apparently unused with ETS managed devices.
  | ConnectionlessManagementConnection
  deriving (Show, Eq, Typeable)

data PacketType
  = InvalidRequest
  | ConnectionInuse
  | ProcessingError
  | Closed
  | ResetConnection
  | OpenBusmonitor
  | OpenBusmonitorText
  | OpenVbusmonitor
  | OpenVbusmonitorText
  | BusmonitorPacket
  | BusmonitorPacketTs
  | OpenBusmonitorTs
  | OpenVbusmonitorTs
  | OpenTConnection
  | OpenTIndividual
  | OpenTGroup
  | OpenTBroadcast
  | OpenTTpdu
  | ApduPacket
  | OpenGroupcon
  | GroupPacket
  | ProgMode
  | MaskVersion
  | MIndividualAddressRead
  | MIndividualAddressWrite
  | ErrorAddrExists
  | ErrorMoreDevice
  | ErrorTimeout
  | ErrorVerify
  | McIndividual
  | McConnection
  | McRead
  | McWrite
  | McPropRead
  | McPropWrite
  | McPeiType
  | McAdcRead
  | McAuthorize
  | McKeyWrite
  | McMaskVersion
  | McRestart
  | McWriteNoverify
  | McProgMode
  | McPropDesc
  | McPropScan
  | LoadImage
  | CacheEnable
  | CacheDisable
  | CacheClear
  | CacheRemove
  | CacheRead
  | CacheReadNowait
  | CacheLastUpdates
  deriving (Show, Eq, Typeable)


$(genSingletons [''PacketDirection, ''ConnectionState, ''PacketType])
$(singDecideInstances [''PacketType])

-- I'm not sure why I have to do this by hand...
deriving instance Show (Sing (d :: PacketDirection))
deriving instance Show (Sing (s :: ConnectionState))
deriving instance Show (Sing (t :: PacketType))

deriving instance Eq (Sing (d :: PacketDirection))
deriving instance Eq (Sing (s :: ConnectionState))
deriving instance Eq (Sing (t :: PacketType))

fromPacketType :: PacketType -> Word16
fromPacketType InvalidRequest = 0
fromPacketType ConnectionInuse = 1
fromPacketType ProcessingError = 2
fromPacketType Closed = 3
fromPacketType ResetConnection = 4
fromPacketType OpenBusmonitor = 16
fromPacketType OpenBusmonitorText = 17
fromPacketType OpenVbusmonitor = 18
fromPacketType OpenVbusmonitorText = 19
fromPacketType BusmonitorPacket = 20
fromPacketType BusmonitorPacketTs = 21
fromPacketType OpenBusmonitorTs = 22
fromPacketType OpenVbusmonitorTs = 23
fromPacketType OpenTConnection = 32
fromPacketType OpenTIndividual = 33
fromPacketType OpenTGroup = 34
fromPacketType OpenTBroadcast = 35
fromPacketType OpenTTpdu = 36
fromPacketType ApduPacket = 37
fromPacketType OpenGroupcon = 38
fromPacketType GroupPacket = 39
fromPacketType ProgMode = 48
fromPacketType MaskVersion = 49
fromPacketType MIndividualAddressRead = 50
fromPacketType MIndividualAddressWrite = 64
fromPacketType ErrorAddrExists = 65
fromPacketType ErrorMoreDevice = 66
fromPacketType ErrorTimeout = 67
fromPacketType ErrorVerify = 68
fromPacketType McIndividual = 73
fromPacketType McConnection = 80
fromPacketType McRead = 81
fromPacketType McWrite = 82
fromPacketType McPropRead = 83
fromPacketType McPropWrite = 84
fromPacketType McPeiType = 85
fromPacketType McAdcRead = 86
fromPacketType McAuthorize = 87
fromPacketType McKeyWrite = 88
fromPacketType McMaskVersion = 89
fromPacketType McRestart = 90
fromPacketType McWriteNoverify = 91
fromPacketType McProgMode = 96
fromPacketType McPropDesc = 97
fromPacketType McPropScan = 98
fromPacketType LoadImage = 99
fromPacketType CacheEnable = 112
fromPacketType CacheDisable = 113
fromPacketType CacheClear = 114
fromPacketType CacheRemove = 115
fromPacketType CacheRead = 116
fromPacketType CacheReadNowait = 117
fromPacketType CacheLastUpdates = 118

toPacketType :: Word16 -> Maybe PacketType      
toPacketType 0 = Just InvalidRequest
toPacketType 1 = Just ConnectionInuse
toPacketType 2 = Just ProcessingError
toPacketType 3 = Just Closed
toPacketType 4 = Just ResetConnection
toPacketType 16 = Just OpenBusmonitor
toPacketType 17 = Just OpenBusmonitorText
toPacketType 18 = Just OpenVbusmonitor
toPacketType 19 = Just OpenVbusmonitorText
toPacketType 20 = Just BusmonitorPacket
toPacketType 21 = Just BusmonitorPacketTs
toPacketType 22 = Just OpenBusmonitorTs
toPacketType 23 = Just OpenVbusmonitorTs
toPacketType 32 = Just OpenTConnection
toPacketType 33 = Just OpenTIndividual
toPacketType 34 = Just OpenTGroup
toPacketType 35 = Just OpenTBroadcast
toPacketType 36 = Just OpenTTpdu
toPacketType 37 = Just ApduPacket
toPacketType 38 = Just OpenGroupcon
toPacketType 39 = Just GroupPacket
toPacketType 48 = Just ProgMode
toPacketType 49 = Just MaskVersion
toPacketType 50 = Just MIndividualAddressRead
toPacketType 64 = Just MIndividualAddressWrite
toPacketType 65 = Just ErrorAddrExists
toPacketType 66 = Just ErrorMoreDevice
toPacketType 67 = Just ErrorTimeout
toPacketType 68 = Just ErrorVerify
toPacketType 73 = Just McIndividual
toPacketType 80 = Just McConnection
toPacketType 81 = Just McRead
toPacketType 82 = Just McWrite
toPacketType 83 = Just McPropRead
toPacketType 84 = Just McPropWrite
toPacketType 85 = Just McPeiType
toPacketType 86 = Just McAdcRead
toPacketType 87 = Just McAuthorize
toPacketType 88 = Just McKeyWrite
toPacketType 89 = Just McMaskVersion
toPacketType 90 = Just McRestart
toPacketType 91 = Just McWriteNoverify
toPacketType 96 = Just McProgMode
toPacketType 97 = Just McPropDesc
toPacketType 98 = Just McPropScan
toPacketType 99 = Just LoadImage
toPacketType 112 = Just CacheEnable
toPacketType 113 = Just CacheDisable
toPacketType 114 = Just CacheClear
toPacketType 115 = Just CacheRemove
toPacketType 116 = Just CacheRead
toPacketType 117 = Just CacheReadNowait
toPacketType 118 = Just CacheLastUpdates
toPacketType _ = Nothing


