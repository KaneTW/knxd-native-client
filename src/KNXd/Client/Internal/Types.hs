{-# LANGUAGE TemplateHaskell, QuasiQuotes, DefaultSignatures #-}
module KNXd.Client.Internal.Types where

import Data.Bits
import Data.Typeable
import Data.Word
import Data.Singletons.TH
import Text.Printf

splitAddress :: Word16 -> (Word16, Word16, Word16)
splitAddress addr = let h = shift addr (-12)
                        m = shift (addr .&. 0x0f00) (-8)
                        l = addr .&. 0xff
                    in (h,m,l)

combineAddress :: Word16 -> Word16 -> Word16 -> Word16
combineAddress h m l = shift (h .&. 0xf) 12 .|. shift (m .&. 0xf) 8 .|. (l .&. 0xff)

-- |An individual, physical address (e.g. 0.0.1)
newtype IndividualAddress = IndividualAddress Word16
                          deriving (Eq, Ord)

instance Show IndividualAddress where
  show (IndividualAddress addr) = let (h,m,l) = splitAddress addr
                                  in printf "%d.%d.%d" h m l

-- |A group address (e.g. 1/2/13)
newtype GroupAddress = GroupAddress Word16
                     deriving (Eq, Ord)

instance Show GroupAddress where
  show (GroupAddress addr) = let (h,m,l) = splitAddress addr
                             in printf "%d/%d/%d" h m l


data PacketDirection
  = FromServer
  | ToServer
  deriving (Show, Eq, Typeable)

data ProgCommand
  = ProgOn
  | ProgOff
  | ProgToggle
  | ProgStatus
  deriving (Show, Eq, Typeable, Enum, Bounded)

data ConnectionState
  = Fresh
  | Broken
  | Busmonitor
  | BusmonitorTs
  | Broadcast
  | Group
  | Individual
  | Tpdu
  | Connection
  | GroupSocket
  | ManagementConnection
  | ConnectionlessManagementConnection
  | Stateless
  deriving (Show, Eq, Typeable, Enum, Bounded)

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

$(genSingletons [''PacketType, ''ConnectionState, ''PacketDirection])

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

toPacketType :: Word16 -> PacketType      
toPacketType 0 = InvalidRequest
toPacketType 1 = ConnectionInuse
toPacketType 2 = ProcessingError
toPacketType 3 = Closed
toPacketType 4 = ResetConnection
toPacketType 16 = OpenBusmonitor
toPacketType 17 = OpenBusmonitorText
toPacketType 18 = OpenVbusmonitor
toPacketType 19 = OpenVbusmonitorText
toPacketType 20 = BusmonitorPacket
toPacketType 21 = BusmonitorPacketTs
toPacketType 22 = OpenBusmonitorTs
toPacketType 23 = OpenVbusmonitorTs
toPacketType 32 = OpenTConnection
toPacketType 33 = OpenTIndividual
toPacketType 34 = OpenTGroup
toPacketType 35 = OpenTBroadcast
toPacketType 36 = OpenTTpdu
toPacketType 37 = ApduPacket
toPacketType 38 = OpenGroupcon
toPacketType 39 = GroupPacket
toPacketType 48 = ProgMode
toPacketType 49 = MaskVersion
toPacketType 50 = MIndividualAddressRead
toPacketType 64 = MIndividualAddressWrite
toPacketType 65 = ErrorAddrExists
toPacketType 66 = ErrorMoreDevice
toPacketType 67 = ErrorTimeout
toPacketType 68 = ErrorVerify
toPacketType 73 = McIndividual
toPacketType 80 = McConnection
toPacketType 81 = McRead
toPacketType 82 = McWrite
toPacketType 83 = McPropRead
toPacketType 84 = McPropWrite
toPacketType 85 = McPeiType
toPacketType 86 = McAdcRead
toPacketType 87 = McAuthorize
toPacketType 88 = McKeyWrite
toPacketType 89 = McMaskVersion
toPacketType 90 = McRestart
toPacketType 91 = McWriteNoverify
toPacketType 96 = McProgMode
toPacketType 97 = McPropDesc
toPacketType 98 = McPropScan
toPacketType 99 = LoadImage
toPacketType 112 = CacheEnable
toPacketType 113 = CacheDisable
toPacketType 114 = CacheClear
toPacketType 115 = CacheRemove
toPacketType 116 = CacheRead
toPacketType 117 = CacheReadNowait
toPacketType 118 = CacheLastUpdates
toPacketType _ = error "unknown packet type"
