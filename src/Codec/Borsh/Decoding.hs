module Codec.Borsh.Decoding (
    -- * Decoders for non-composite types mandated by the Borsh spec
    decodeU8
  , decodeU16
  , decodeU32
  , decodeU64
  , decodeU128
  , decodeI8
  , decodeI16
  , decodeI32
  , decodeI64
  , decodeI128
  , decodeF32
  , decodeF64
  , decodeString
    -- * Decoders for composite types mandated by the Borsh spec
  , decodeArray
  , decodeVec
  , decodeOption
  , decodeHashSet
  , decodeHashMap
  , decodeStruct
  , decodeEnum
    -- * Decoders for Haskell types not mandated by the Borsh spec
  , decodeLazyByteString
  , decodeStrictByteString
  , decodeChar
  , decodeBool
  ) where

import Data.Char (chr)
import Data.Int
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import Data.STRef
import Data.Text (Text)
import Data.Word
import Generics.SOP
import GHC.TypeLits

import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.Text.Encoding          as Text
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

import Codec.Borsh.Incremental
import Codec.Borsh.Internal.Util.BitwiseCast
import Codec.Borsh.Internal.Util.SOP
import Data.FixedSizeArray (FixedSizeArray, MFixedSizeArray)
import Data.Word128 (Word128)
import Data.Int128 (Int128)

import qualified Data.FixedSizeArray as FSA

{-------------------------------------------------------------------------------
  Decoders for the non-composite types mandated by the Borsh spec
-------------------------------------------------------------------------------}

decodeU8   :: Decoder s Word8
decodeU16  :: Decoder s Word16
decodeU32  :: Decoder s Word32
decodeU64  :: Decoder s Word64
decodeU128 :: Decoder s Word128

decodeU8   = decodeLittleEndian
decodeU16  = decodeLittleEndian
decodeU32  = decodeLittleEndian
decodeU64  = decodeLittleEndian
decodeU128 = decodeLittleEndian

decodeI8   :: Decoder s Int8
decodeI16  :: Decoder s Int16
decodeI32  :: Decoder s Int32
decodeI64  :: Decoder s Int64
decodeI128 :: Decoder s Int128

decodeI8   = (castBits @Word8  ) <$> decodeLittleEndian
decodeI16  = (castBits @Word16 ) <$> decodeLittleEndian
decodeI32  = (castBits @Word32 ) <$> decodeLittleEndian
decodeI64  = (castBits @Word64 ) <$> decodeLittleEndian
decodeI128 = (castBits @Word128) <$> decodeLittleEndian

decodeF32  :: Decoder s Float
decodeF64  :: Decoder s Double

decodeF32  = (castBits @Word32) <$> decodeLittleEndian
decodeF64  = (castBits @Word64) <$> decodeLittleEndian

decodeString :: Decoder s Text
decodeString = do
    len <- decodeU32
    lbs <- decodeLargeToken len
    case Text.decodeUtf8' $ L.toStrict lbs of
      Right txt -> return txt
      Left  err -> fail (show err)

{-------------------------------------------------------------------------------
  Decoders for composite types mandated by the Borsh spec
-------------------------------------------------------------------------------}

decodeArray :: forall n s a.
     KnownNat n
  => Decoder s a -> Decoder s (FixedSizeArray n a)
decodeArray d = do
    -- Construct mutable array before we start processing elements,
    -- along with a counter for the next element
    mArr :: MFixedSizeArray n s a <- liftDecoder $ FSA.new
    next :: STRef s Int           <- liftDecoder $ newSTRef 0

    let d' :: Decoder s ()
        d' = d >>= \b -> liftDecoder $ do
                i <- readSTRef next
                modifySTRef next (+ 1)
                GM.write mArr i b

    decodeIncremental_ count d'
    liftDecoder $ G.freeze mArr
  where
    count :: Word32
    count = fromIntegral $ natVal (Proxy @n)

decodeVec :: Decoder s a -> Decoder s [a]
decodeVec d = decodeU32 >>= \count -> decodeIncremental count d

decodeOption :: Decoder s a -> Decoder s (Maybe a)
decodeOption d = do
    present <- decodeU8
    case present of
      0 -> return Nothing
      1 -> Just <$> d
      _ -> fail "Expected 0 or 1 for option prefix"

decodeHashSet :: Ord a => Decoder s a -> Decoder s (Set a)
decodeHashSet d = do
    count <- decodeU32
    Set.fromList <$> decodeIncremental count d

decodeHashMap :: Ord k => Decoder s k -> Decoder s a -> Decoder s (Map k a)
decodeHashMap dk dv = do
    count <- decodeU32
    Map.fromList <$> decodeIncremental count dPair
  where
    dPair = (,) <$> dk <*> dv

decodeStruct :: All Top xs => NP (Decoder s) xs -> Decoder s (NP I xs)
decodeStruct = hsequence

decodeEnum :: forall s xss.
     All SListI xss
  => POP (Decoder s) xss -> Decoder s (SOP I xss)
decodeEnum =
      selectDecoder
    . hcollapse
    . hczipWith3
        (Proxy @SListI)
        (\(K ix) (Fn inj) ds -> K $ (ix, SOP . unK . inj <$> hsequence ds))
        indices
        (injections :: NP (Injection (NP I) xss) xss)
    . unPOP
  where
    selectDecoder :: [(Word8, Decoder s (SOP I xss))] -> Decoder s (SOP I xss)
    selectDecoder decs = do
        n <- fromIntegral <$> decodeU8
        fromMaybe (fail err) $ lookup n decs
      where
        err :: String
        err = "Expected index < " ++ show (length decs)

{-------------------------------------------------------------------------------
  Decoders for Haskell types not mandated by the Borsh spec
-------------------------------------------------------------------------------}

-- ByteStrings

decodeLazyByteString :: Decoder s L.ByteString
decodeLazyByteString = do
    len <- decodeU32
    decodeLargeToken len

decodeStrictByteString :: Decoder s S.ByteString
decodeStrictByteString = do
    len <- decodeU32
    L.toStrict <$> decodeLargeToken len

-- Char, Bool

decodeChar :: Decoder s Char
decodeChar = chr . fromIntegral <$> decodeU32

decodeBool :: Decoder s Bool
decodeBool = decodeU8 >>= \case
    0 -> return False
    1 -> return True
    _ -> fail "Expected 0 or 1 while decoding Bool"
