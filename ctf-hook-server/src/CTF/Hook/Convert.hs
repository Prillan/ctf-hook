module CTF.Hook.Convert ( ToString
                        , convert
                        , toString) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as LB
import           Data.String             (IsString (..))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LE

class Convert a b where
  convert :: a -> b

instance Convert T.Text LT.Text where
  convert = LT.fromStrict

instance Convert LT.Text T.Text where
  convert = LT.toStrict

instance Convert B.ByteString LB.ByteString where
  convert = LB.fromStrict

instance Convert LB.ByteString B.ByteString where
  convert = LB.toStrict

instance (a ~ Char) => Convert T.Text [a] where
  convert = T.unpack
instance (a ~ Char) => Convert LT.Text [a] where
  convert = LT.unpack

instance (a ~ Char) => Convert [a] T.Text where
  convert = T.pack
instance (a ~ Char) => Convert [a] LT.Text where
  convert = LT.pack

instance Convert LB.ByteString LT.Text where
  convert = LE.decodeUtf8

instance Convert LB.ByteString T.Text where
  convert = LT.toStrict . LE.decodeUtf8

instance Convert B.ByteString T.Text where
  convert = E.decodeUtf8

instance Convert LT.Text LB.ByteString where
  convert = LE.encodeUtf8
instance Convert T.Text B.ByteString where
  convert = E.encodeUtf8

instance (a ~ Char) => Convert [a] B.ByteString where
  convert = fromString
instance (a ~ Char) => Convert [a] LB.ByteString where
  convert = fromString

instance (a ~ Char) => Convert B.ByteString [a] where
  convert = T.unpack . E.decodeUtf8
instance (a ~ Char) => Convert LB.ByteString [a] where
  convert = LT.unpack . LE.decodeUtf8

class ToString a where
  toString :: a -> String

instance (a ~ Char) => ToString [a] where
  toString = id

instance ToString T.Text where
  toString = T.unpack

instance ToString LT.Text where
  toString = LT.unpack

instance ToString B.ByteString where
  toString = toString . E.decodeUtf8

instance ToString LB.ByteString where
  toString = toString . LE.decodeUtf8
