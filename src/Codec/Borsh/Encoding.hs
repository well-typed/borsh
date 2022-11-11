module Codec.Borsh.Encoding (
    -- * Encoder definition
    Encoder (..)
    -- * Encoders for non-composite types mandated by the Borsh spec
  , encodeU8
  , encodeU16
  , encodeU32
  , encodeU64
  , encodeU128
  , encodeI8
  , encodeI16
  , encodeI32
  , encodeI64
  , encodeI128
  , encodeF32
  , encodeF64
  , encodeString
  -- * Encoders for composite types mandated by the Borsh spec
  , encodeArray
  , encodeVec
  , encodeOption
  , encodeHashSet
  , encodeHashMap
  , encodeStruct
  , encodeEnum
  -- * Encoders for Haskell types not mandated by the Borsh spec
  , encodeLazyByteString
  , encodeStrictByteString
  , encodeChar
  , encodeBool
  ) where

import Data.Char (ord)
import Data.ByteString.Builder (Builder)
import Data.Foldable (toList)
import Data.Functor.Contravariant
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.SOP
import Data.Text (Text)
import Data.Word

import qualified Data.ByteString         as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as Text

import Data.FixedSizeArray (FixedSizeArray)
import Codec.Borsh.Internal.Util.ByteString
import Codec.Borsh.Internal.Util.SOP (indices)
import Data.Word128
import Data.Int128

{-------------------------------------------------------------------------------
  Encoder definition
-------------------------------------------------------------------------------}

-- | Encoder
--
-- An encoder describes how to serialise a given value in BORSH format.
newtype Encoder a = Encoder {
      runEncoder :: a -> Builder
    }

instance Contravariant Encoder where
  contramap f (Encoder e) = Encoder (e . f)

liftEncoder :: Encoder a -> (I -.-> K Builder) a
liftEncoder (Encoder e) = fn $ K . e . unI

{-------------------------------------------------------------------------------
  Encoders for non-composite types mandated by the Borsh spec
-------------------------------------------------------------------------------}

encodeU8   :: Encoder Word8
encodeU16  :: Encoder Word16
encodeU32  :: Encoder Word32
encodeU64  :: Encoder Word64
encodeI8   :: Encoder Int8
encodeI16  :: Encoder Int16
encodeI32  :: Encoder Int32
encodeI64  :: Encoder Int64
encodeF32  :: Encoder Float
encodeF64  :: Encoder Double

encodeU8  = Encoder B.word8
encodeU16 = Encoder B.word16LE
encodeU32 = Encoder B.word32LE
encodeU64 = Encoder B.word64LE
encodeI8  = Encoder B.int8
encodeI16 = Encoder B.int16LE
encodeI32 = Encoder B.int32LE
encodeI64 = Encoder B.int64LE
encodeF32 = Encoder B.floatLE
encodeF64 = Encoder B.doubleLE

encodeU128 :: Encoder Word128
encodeU128 = Encoder $
    \w128 -> B.word64LE (word128LS64 w128) <> B.word64LE (word128MS64 w128)

encodeI128 :: Encoder Int128
encodeI128 = Encoder $
    \i128 -> B.word64LE (int128LS64 i128) <> B.word64LE (int128MS64 i128)

-- Encoding 'Text'
--
-- Borsh requires the length of the utf8-encoded string before the string, but
-- unfortunately we have no easy way to compute this without encoding the entire
-- string. This means that we are not streaming here: the entire utf8 encoding
-- is constructed in memory.
--
-- With text version 2.0 we can use @lengthWord8@ but that is not available most
-- of the time.
encodeString :: Encoder Text
encodeString = Encoder $ \txt ->
    B.word32LE (lengthLazy $ utf8 txt) <> B.lazyByteString (utf8 txt)
  where
    utf8 :: Text -> L.ByteString
    utf8 txt = B.toLazyByteString $ Text.encodeUtf8Builder txt

{-------------------------------------------------------------------------------
  Encoders for composite types mandated by the Borsh spec
-------------------------------------------------------------------------------}

encodeArray :: Encoder a -> Encoder (FixedSizeArray n a)
encodeArray e = Encoder $ mconcat . map (runEncoder e) . toList

encodeVec :: Encoder a -> Encoder [a]
encodeVec e = Encoder $ \xs -> mconcat $
      runEncoder encodeU32 (fromIntegral $ length xs)
    : map (runEncoder e) xs

encodeOption :: Encoder a -> Encoder (Maybe a)
encodeOption e = Encoder $ \case
    Nothing -> runEncoder encodeU8 0
    Just x  -> runEncoder encodeU8 1 <> runEncoder e x

encodeHashSet :: Encoder a -> Encoder (Set a)
encodeHashSet e = Encoder $ \xs -> mconcat $
      runEncoder encodeU32 (fromIntegral $ Set.size xs)
    : (map (runEncoder e) $ Set.toList xs)

encodeHashMap :: Encoder k -> Encoder a -> Encoder (Map k a)
encodeHashMap ek ev = Encoder $ \xs -> mconcat $
      runEncoder encodeU32 (fromIntegral $ Map.size xs)
    : (map ePair $ Map.toList xs)
  where
    ePair (k,v) =  runEncoder ek k <> runEncoder ev v

encodeStruct :: SListI xs => NP Encoder xs -> Encoder (NP I xs)
encodeStruct es = Encoder $
      mconcat
    . hcollapse
    . hap (hliftA liftEncoder es)

encodeEnum :: All SListI xss => POP Encoder xss -> Encoder (SOP I xss)
encodeEnum  es = Encoder $
      hcollapse
    . hczipWith (Proxy @SListI) aux indices
    . unSOP
    . hap (hliftA liftEncoder es)
  where
    aux :: SListI xs => K Word8 xs -> NP (K Builder) xs -> K Builder xs
    aux (K ix) xs = K $ runEncoder encodeU8 ix <> mconcat (hcollapse xs)

{-------------------------------------------------------------------------------
  Encoders for Haskell types not mandated by the Borsh spec
-------------------------------------------------------------------------------}

-- ByteStrings

encodeLazyByteString :: Encoder L.ByteString
encodeLazyByteString = Encoder $ \bs ->
       runEncoder encodeU32 (fromIntegral $ L.length bs)
    <> B.lazyByteString bs

encodeStrictByteString :: Encoder S.ByteString
encodeStrictByteString = Encoder $ \bs ->
       runEncoder encodeU32 (fromIntegral $ S.length bs)
    <> B.byteString bs

-- Char, Bool

encodeChar :: Encoder Char
encodeChar = Encoder $ runEncoder encodeU32 . fromIntegral . ord

encodeBool :: Encoder Bool
encodeBool = Encoder $ runEncoder encodeU8 . fromIntegral . fromEnum

