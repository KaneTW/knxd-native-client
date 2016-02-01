module KNXd.Client.Internal.Serialize where

import Data.ByteString (ByteString)
import Data.HList
import Data.Serialize
import Data.Word
import KNXd.Client.Internal.Types

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
  -- |Note that this uses 'putByteString' to avoid writing length
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
