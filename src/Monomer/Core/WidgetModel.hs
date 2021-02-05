{-# LANGUAGE FlexibleInstances #-}

module Monomer.Core.WidgetModel where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.Serialise
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Data.Word
import Foreign.C.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Exception

class Typeable s => WidgetModel s where
  modelToByteString :: s -> ByteString
  modelToByteString _ = throw (AssertionFailed "Model serialization not implemented")
  byteStringToModel :: ByteString -> Either String s
  byteStringToModel _ = throw (AssertionFailed "Model deserialization not implemented")

bsToSerialiseModel :: Serialise s => ByteString -> Either String s
bsToSerialiseModel bs = case deserialiseOrFail bs of
  Right val -> Right val
  Left err -> Left (show err)

instance WidgetModel a => WidgetModel (Maybe a) where
  modelToByteString val = serialise (modelToByteString <$> val)
  byteStringToModel bs = deserialise bs >>= byteStringToModel

instance WidgetModel () where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Integer where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Int where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Int8 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Int16 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Int32 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Int64 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Word where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Word8 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Word16 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Word32 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Word64 where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Float where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Double where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel CFloat where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel CDouble where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Rational where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel BS.ByteString where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel BSL.ByteString where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

instance WidgetModel Text where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel
