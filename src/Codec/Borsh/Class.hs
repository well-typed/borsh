{-# LANGUAGE PolyKinds #-}

module Codec.Borsh.Class (
    -- * Serialisation
    ToBorsh(..)
  , FromBorsh(..)
    -- ** Deriving-via support
  , AsEnum(..)
  , AsStruct(..)
  , KnownImpliesMax(..)
    -- * Size information
  , KnownSize(..)
  , Size(..)
  , BorshSize(..)
  , BorshSizeSum(..)
  , BorshMaxSize(..)
    -- * Derived functionality
  , serialiseBorsh
  , deserialiseBorsh
  ) where

import Data.Functor.Contravariant
import Data.Int
import Data.Kind
import Data.Map (Map)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import Generics.SOP
import GHC.TypeNats

import qualified Data.ByteString         as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L

import Codec.Borsh.Decoding
import Codec.Borsh.Encoding
import Codec.Borsh.Incremental
import Data.FixedSizeArray (FixedSizeArray)

{-------------------------------------------------------------------------------
  Size information

  We do not try to compute the size at the type-level (we don't need to and
  this would get messy), but we /do/ record at the type level whether the
  size is statically known or not.
-------------------------------------------------------------------------------}

data KnownSize = HasKnownSize | HasVariableSize

-- | The statically known size of encodings of values of a particular type.
data Size (a :: KnownSize) where
  SizeKnown    :: Word32 -> Size 'HasKnownSize
  SizeVariable :: Size 'HasVariableSize

deriving instance Show (Size a)
deriving instance Eq   (Size a)

class BorshSize (a :: Type) where
  type StaticBorshSize a :: KnownSize

  -- | Size of the Borsh encoding, if known statically (independent of a value)
  --
  -- There is no generic default implementation of 'borshSize'; instead use
  -- deriving-via using 'AsStruct' or 'AsEnum'.
  borshSize :: Proxy a -> Size (StaticBorshSize a)

class BorshMaxSize (a :: Type) where
  -- | Maximum size of the Borsh encoding
  --
  -- There is no generic default implementation of 'borshMaxSize'; instead use
  -- deriving-via using 'AsStruct' or 'AsEnum'.
  --
  -- However, while it is possible to use deriving-via to derive 'BorshMaxSize'
  -- for your own types, recursive types should /not/ be given an instance (and
  -- the derived function will not terminate).
  borshMaxSize :: Proxy a -> Word32

-- | If the size of a type's Borsh encoding is statically known then we also
-- know the maximum size of the encoding. Useful for deriving-via.
newtype KnownImpliesMax a = KnownImpliesMax { getKnownImpliesMax :: a }

instance ( BorshSize a
         , StaticBorshSize a ~ 'HasKnownSize
         ) => BorshMaxSize (KnownImpliesMax a) where
  borshMaxSize _ = case borshSize (Proxy @a) of SizeKnown n -> n

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class BorshSize a => ToBorsh a where
  -- | Encoder to Borsh
  --
  -- There is no generic default implementation of 'encodeBorsh'; instead use
  -- deriving-via using 'AsStruct' or 'AsEnum'.
  encodeBorsh :: Encoder a

class BorshSize a => FromBorsh a where
  -- | Decode from Borsh
  --
  -- There is no generic default implementation of 'decodeBorsh'; instead use
  -- deriving-via using 'AsStruct' or 'AsEnum'.
  decodeBorsh :: Decoder s a

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

-- | Deriving-via support for enums (general ADTs)
--
-- The Borsh spec <https://borsh.io/> mandates that enums have a tag indicating
-- the constructor, even when there is only a single constructor in the enum.
-- In Rust this makes more sense than in Haskell, since in Rust enums and
-- structs are introduced through different keywords. In Haskell, of course,
-- the only difference between them is that a struct is an enum with a single
-- constructor.
--
-- The generic encoder en decoder you get in 'ToBorsh' and 'FromBorsh' when
-- deriving via @AsEnum@ will therefore add the tag, independent of the number of
-- constructors:
--
-- > data MyEnum = ..
-- >   deriving (BorshSize, ToBorsh, FromBorsh) via AsEnum MyEnum
--
-- If you want the encoding of a struct, without the tag, you need to derive via
-- 'AsStruct'.
newtype AsEnum a = AsEnum { getEnum :: a }

instance BorshSizeSum (Code a) => BorshSize (AsEnum a) where
  type StaticBorshSize (AsEnum a) = SumKnownSize (Code a)
  borshSize _ = borshSizeSum (Proxy @(Code a))

instance ( Generic a
         , All2 BorshMaxSize (Code a)
         ) => BorshMaxSize (AsEnum a) where
  borshMaxSize _ = borshMaxSize (Proxy @(SOP I (Code a)))

instance ( Generic a
         , BorshSizeSum (Code a)
         , All2 ToBorsh (Code a)
         ) => ToBorsh (AsEnum a) where
  encodeBorsh = contramap (from . getEnum) encodeBorsh

instance ( Generic a
         , BorshSizeSum  (Code a)
         , All2 FromBorsh (Code a)
         ) => FromBorsh (AsEnum a) where
  decodeBorsh = fmap (AsEnum . to) decodeBorsh

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Deriving-via support for structs
--
-- Usage:
--
-- > data MyStruct = ..
-- >   deriving (BorshSize, ToBorsh, FromBorsh) via AsStruct MyStruct
--
-- NOTE: Doing so may have consequences for forwards compatibility: if a tag
-- is present, additional constructors can be added without invalidating the
-- encoding of existing constructors. See also 'AsEnum'.
newtype AsStruct a = AsStruct { getStruct :: a }

instance ( IsProductType a xs
         , All BorshSize xs
         ) => BorshSize (AsStruct a) where
  type StaticBorshSize (AsStruct a) = ProdKnownSize (ProductCode a)
  borshSize _ = sizeOfProd (Proxy @(ProductCode a))

instance ( IsProductType a xs
         , All BorshMaxSize xs
         ) => BorshMaxSize (AsStruct a) where
  borshMaxSize _ = borshMaxSize (Proxy @(NP I (ProductCode a)))

instance ( IsProductType a xs
         , All BorshSize xs
         , All ToBorsh xs
         ) => ToBorsh (AsStruct a) where
  encodeBorsh = contramap (productTypeFrom . getStruct) encodeBorsh

instance ( IsProductType a xs
         , All BorshSize xs
         , All FromBorsh xs
         ) => FromBorsh (AsStruct a) where
  decodeBorsh = fmap (AsStruct . productTypeTo) decodeBorsh

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

serialiseBorsh :: ToBorsh a => a -> L.ByteString
serialiseBorsh = B.toLazyByteString . runEncoder encodeBorsh

deserialiseBorsh :: FromBorsh a => L.ByteString -> Either DeserialiseFailure a
deserialiseBorsh bs =
    aux <$> deserialiseByteString decodeBorsh bs
  where
    aux (_leftover, _offset, a) = a

{-------------------------------------------------------------------------------
  Sizes
-------------------------------------------------------------------------------}

instance BorshSize Word8 where
  type StaticBorshSize Word8 = 'HasKnownSize
  borshSize _ = SizeKnown 1

instance BorshSize Word16 where
  type StaticBorshSize Word16 = 'HasKnownSize
  borshSize _ = SizeKnown 2

instance BorshSize Word32 where
  type StaticBorshSize Word32 = 'HasKnownSize
  borshSize _ = SizeKnown 4

instance BorshSize Word64 where
  type StaticBorshSize Word64 = 'HasKnownSize
  borshSize _ = SizeKnown 8

instance BorshSize Word128 where
  type StaticBorshSize Word128 = 'HasKnownSize
  borshSize _ = SizeKnown 16

instance BorshSize Int8 where
  type StaticBorshSize Int8 = 'HasKnownSize
  borshSize _ = SizeKnown 1

instance BorshSize Int16 where
  type StaticBorshSize Int16 = 'HasKnownSize
  borshSize _ = SizeKnown 2

instance BorshSize Int32 where
  type StaticBorshSize Int32 = 'HasKnownSize
  borshSize _ = SizeKnown 4

instance BorshSize Int64 where
  type StaticBorshSize Int64 = 'HasKnownSize
  borshSize _ = SizeKnown 8

instance BorshSize Int128 where
  type StaticBorshSize Int128 = 'HasKnownSize
  borshSize _ = SizeKnown 16

instance BorshSize Float where
  type StaticBorshSize Float = 'HasKnownSize
  borshSize _ = SizeKnown 4

instance BorshSize Double where
  type StaticBorshSize Double = 'HasKnownSize
  borshSize _ = SizeKnown 8

instance (KnownNat n, BorshSize a) => BorshSize (FixedSizeArray n a) where
  type StaticBorshSize (FixedSizeArray n a) = StaticBorshSize a

  borshSize _ =
      case borshSize (Proxy @a) of
        SizeVariable -> SizeVariable
        SizeKnown n  -> SizeKnown (n * fromIntegral (natVal (Proxy @n)))

instance BorshSize Text where
  type StaticBorshSize Text = 'HasVariableSize
  borshSize _ = SizeVariable

deriving via AsEnum [a]       instance BorshSize [a]
deriving via AsEnum (Maybe a) instance BorshSize (Maybe a)

instance BorshSize (Set a) where
  type StaticBorshSize (Set a) = 'HasVariableSize
  borshSize _ = SizeVariable

instance BorshSize (Map k a) where
  type StaticBorshSize (Map k a) = 'HasVariableSize
  borshSize _ = SizeVariable

instance All BorshSize xs => BorshSize (NP I xs) where
  type StaticBorshSize (NP I xs) = ProdKnownSize xs
  borshSize _ = sizeOfProd (Proxy @xs)

instance BorshSizeSum xss => BorshSize (SOP I xss) where
  type StaticBorshSize (SOP I xss) = SumKnownSize xss
  borshSize _ = borshSizeSum (Proxy @xss)

{-------------------------------------------------------------------------------
  Maximum sizes
-------------------------------------------------------------------------------}

deriving via KnownImpliesMax Word8   instance BorshMaxSize Word8
deriving via KnownImpliesMax Word16  instance BorshMaxSize Word16
deriving via KnownImpliesMax Word32  instance BorshMaxSize Word32
deriving via KnownImpliesMax Word64  instance BorshMaxSize Word64
deriving via KnownImpliesMax Word128 instance BorshMaxSize Word128

deriving via KnownImpliesMax Int8   instance BorshMaxSize Int8
deriving via KnownImpliesMax Int16  instance BorshMaxSize Int16
deriving via KnownImpliesMax Int32  instance BorshMaxSize Int32
deriving via KnownImpliesMax Int64  instance BorshMaxSize Int64
deriving via KnownImpliesMax Int128 instance BorshMaxSize Int128

deriving via KnownImpliesMax Float  instance BorshMaxSize Float
deriving via KnownImpliesMax Double instance BorshMaxSize Double

instance ( KnownNat n
         , BorshMaxSize a
         ) => BorshMaxSize (FixedSizeArray n a) where
  borshMaxSize _ = fromIntegral (natVal (Proxy @n)) * borshMaxSize (Proxy @a)

deriving via AsEnum (Maybe a) instance BorshMaxSize a => BorshMaxSize (Maybe a)

instance (All2 BorshMaxSize xss, All SListI xss) => BorshMaxSize (SOP I xss) where
  borshMaxSize _ = aux . hcollapse $ sizes
    where
      aux :: [[Word32]] -> Word32
      aux [] = 1
      aux xs = 1 + maximum (map sum xs)

      borshMaxSize' :: forall a. (BorshMaxSize a) => K Word32 a
      borshMaxSize' = K $ borshMaxSize (Proxy @a)

      sizes :: POP (K Word32) xss
      sizes = hcpure (Proxy @BorshMaxSize) borshMaxSize'

instance (All BorshMaxSize xs) => BorshMaxSize (NP I xs) where
  borshMaxSize _ = sum . hcollapse $ sizes
    where
      borshMaxSize' :: forall a. (BorshMaxSize a) => K Word32 a
      borshMaxSize' = K $ borshMaxSize (Proxy @a)

      sizes :: NP (K Word32) xs
      sizes = hcpure (Proxy @BorshMaxSize) borshMaxSize'

{-------------------------------------------------------------------------------
  ToBorsh instances
-------------------------------------------------------------------------------}

instance ToBorsh Word8   where encodeBorsh = encodeU8
instance ToBorsh Word16  where encodeBorsh = encodeU16
instance ToBorsh Word32  where encodeBorsh = encodeU32
instance ToBorsh Word64  where encodeBorsh = encodeU64
instance ToBorsh Word128 where encodeBorsh = encodeU128
instance ToBorsh Int8    where encodeBorsh = encodeI8
instance ToBorsh Int16   where encodeBorsh = encodeI16
instance ToBorsh Int32   where encodeBorsh = encodeI32
instance ToBorsh Int64   where encodeBorsh = encodeI64
instance ToBorsh Int128  where encodeBorsh = encodeI128
instance ToBorsh Float   where encodeBorsh = encodeF32
instance ToBorsh Double  where encodeBorsh = encodeF64
instance ToBorsh Text    where encodeBorsh = encodeString

instance (KnownNat n, ToBorsh a) => ToBorsh (FixedSizeArray n a) where
  encodeBorsh = encodeArray encodeBorsh

instance ToBorsh a => ToBorsh [a] where
  encodeBorsh = encodeVec encodeBorsh

instance ToBorsh a => ToBorsh (Maybe a) where
  encodeBorsh = encodeOption encodeBorsh

instance ToBorsh a => ToBorsh (Set a) where
  encodeBorsh = encodeHashSet encodeBorsh

instance (ToBorsh k, ToBorsh a) => ToBorsh (Map k a) where
  encodeBorsh = encodeHashMap encodeBorsh encodeBorsh

instance (All BorshSize xs, All ToBorsh xs) => ToBorsh (NP I xs) where
  encodeBorsh = encodeStruct $ hcpure (Proxy @ToBorsh) encodeBorsh

instance ( BorshSizeSum xss
         , All2 ToBorsh xss
         , All SListI xss
         ) => ToBorsh (SOP I xss) where
  encodeBorsh = encodeEnum $ hcpure (Proxy @ToBorsh) encodeBorsh

{-------------------------------------------------------------------------------
  FromBorsh instances
-------------------------------------------------------------------------------}

instance FromBorsh Word8   where decodeBorsh = decodeU8
instance FromBorsh Word16  where decodeBorsh = decodeU16
instance FromBorsh Word32  where decodeBorsh = decodeU32
instance FromBorsh Word64  where decodeBorsh = decodeU64
instance FromBorsh Word128 where decodeBorsh = decodeU128
instance FromBorsh Int8    where decodeBorsh = decodeI8
instance FromBorsh Int16   where decodeBorsh = decodeI16
instance FromBorsh Int32   where decodeBorsh = decodeI32
instance FromBorsh Int64   where decodeBorsh = decodeI64
instance FromBorsh Int128  where decodeBorsh = decodeI128
instance FromBorsh Float   where decodeBorsh = decodeF32
instance FromBorsh Double  where decodeBorsh = decodeF64
instance FromBorsh Text    where decodeBorsh = decodeString

instance FromBorsh a => FromBorsh [a] where
  decodeBorsh = decodeVec decodeBorsh

instance (FromBorsh a, KnownNat n) => FromBorsh (FixedSizeArray n a) where
  decodeBorsh = decodeArray decodeBorsh

instance FromBorsh a => FromBorsh (Maybe a) where
  decodeBorsh = decodeOption decodeBorsh

instance (FromBorsh a, Ord a) => FromBorsh (Set a) where
  decodeBorsh = decodeHashSet decodeBorsh

instance
     (FromBorsh k, FromBorsh a, Ord k)
  => FromBorsh (Map k a) where
  decodeBorsh = decodeHashMap decodeBorsh decodeBorsh

instance (All BorshSize xs, All FromBorsh xs) => FromBorsh (NP I xs) where
  decodeBorsh = decodeStruct $ hcpure (Proxy @FromBorsh) decodeBorsh

instance ( BorshSizeSum xss
         , All SListI xss
         , All2 FromBorsh xss
         ) => FromBorsh (SOP I xss) where
  decodeBorsh = decodeEnum $ hcpure (Proxy @FromBorsh) decodeBorsh

{-------------------------------------------------------------------------------
  Instances for tuples
-------------------------------------------------------------------------------}

-- size 0

deriving via AsStruct () instance BorshSize    ()
deriving via AsStruct () instance BorshMaxSize ()
deriving via AsStruct () instance ToBorsh      ()
deriving via AsStruct () instance FromBorsh    ()

-- size 2

deriving via AsStruct (a, b)
         instance
              ( BorshSize a
              , BorshSize b
              )
           => BorshSize (a, b)
deriving via AsStruct (a, b)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              )
           => BorshMaxSize (a, b)
deriving via AsStruct (a, b)
         instance
              ( ToBorsh a
              , ToBorsh b
              )
           => ToBorsh (a, b)
deriving via AsStruct (a, b)
         instance
              ( FromBorsh a
              , FromBorsh b
              )
           => FromBorsh (a, b)

-- size 3

deriving via AsStruct (a, b, c)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              )
           => BorshSize (a, b, c)
deriving via AsStruct (a, b, c)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              )
           => BorshMaxSize (a, b, c)
deriving via AsStruct (a, b, c)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              )
           => ToBorsh (a, b, c)
deriving via AsStruct (a, b, c)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              )
           => FromBorsh (a, b, c)

-- size 4

deriving via AsStruct (a, b, c, d)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              )
           => BorshSize (a, b, c, d)
deriving via AsStruct (a, b, c, d)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              )
           => BorshMaxSize (a, b, c, d)
deriving via AsStruct (a, b, c, d)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              )
           => ToBorsh (a, b, c, d)
deriving via AsStruct (a, b, c, d)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              )
           => FromBorsh (a, b, c, d)

-- size 5

deriving via AsStruct (a, b, c, d, e)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              )
           => BorshSize (a, b, c, d, e)
deriving via AsStruct (a, b, c, d, e)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              )
           => BorshMaxSize (a, b, c, d, e)
deriving via AsStruct (a, b, c, d, e)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              )
           => ToBorsh (a, b, c, d, e)
deriving via AsStruct (a, b, c, d, e)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              )
           => FromBorsh (a, b, c, d, e)

-- size 6

deriving via AsStruct (a, b, c, d, e, f)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              )
           => BorshSize (a, b, c, d, e, f)
deriving via AsStruct (a, b, c, d, e, f)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              , BorshMaxSize f
              )
           => BorshMaxSize (a, b, c, d, e, f)
deriving via AsStruct (a, b, c, d, e, f)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              )
           => ToBorsh (a, b, c, d, e, f)
deriving via AsStruct (a, b, c, d, e, f)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              , FromBorsh f
              )
           => FromBorsh (a, b, c, d, e, f)

-- size 7

deriving via AsStruct (a, b, c, d, e, f, g)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              , BorshSize g
              )
           => BorshSize (a, b, c, d, e, f, g)
deriving via AsStruct (a, b, c, d, e, f, g)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              , BorshMaxSize f
              , BorshMaxSize g
              )
           => BorshMaxSize (a, b, c, d, e, f, g)
deriving via AsStruct (a, b, c, d, e, f, g)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              , ToBorsh g
              )
           => ToBorsh (a, b, c, d, e, f, g)
deriving via AsStruct (a, b, c, d, e, f, g)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              , FromBorsh f
              , FromBorsh g
              )
           => FromBorsh (a, b, c, d, e, f, g)

-- size 8

deriving via AsStruct (a, b, c, d, e, f, g, h)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              , BorshSize g
              , BorshSize h
              )
           => BorshSize (a, b, c, d, e, f, g, h)
deriving via AsStruct (a, b, c, d, e, f, g, h)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              , BorshMaxSize f
              , BorshMaxSize g
              , BorshMaxSize h
              )
           => BorshMaxSize (a, b, c, d, e, f, g, h)
deriving via AsStruct (a, b, c, d, e, f, g, h)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              , ToBorsh g
              , ToBorsh h
              )
           => ToBorsh (a, b, c, d, e, f, g, h)
deriving via AsStruct (a, b, c, d, e, f, g, h)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              , FromBorsh f
              , FromBorsh g
              , FromBorsh h
              )
           => FromBorsh (a, b, c, d, e, f, g, h)

-- size 9

deriving via AsStruct (a, b, c, d, e, f, g, h, i)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              , BorshSize g
              , BorshSize h
              , BorshSize i
              )
           => BorshSize (a, b, c, d, e, f, g, h, i)
deriving via AsStruct (a, b, c, d, e, f, g, h, i)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              , BorshMaxSize f
              , BorshMaxSize g
              , BorshMaxSize h
              , BorshMaxSize i
              )
           => BorshMaxSize (a, b, c, d, e, f, g, h, i)
deriving via AsStruct (a, b, c, d, e, f, g, h, i)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              , ToBorsh g
              , ToBorsh h
              , ToBorsh i
              )
           => ToBorsh (a, b, c, d, e, f, g, h, i)
deriving via AsStruct (a, b, c, d, e, f, g, h, i)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              , FromBorsh f
              , FromBorsh g
              , FromBorsh h
              , FromBorsh i
              )
           => FromBorsh (a, b, c, d, e, f, g, h, i)

-- size 10

deriving via AsStruct (a, b, c, d, e, f, g, h, i, j)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              , BorshSize g
              , BorshSize h
              , BorshSize i
              , BorshSize j
              )
           => BorshSize (a, b, c, d, e, f, g, h, i, j)
deriving via AsStruct (a, b, c, d, e, f, g, h, i, j)
         instance
              ( BorshMaxSize a
              , BorshMaxSize b
              , BorshMaxSize c
              , BorshMaxSize d
              , BorshMaxSize e
              , BorshMaxSize f
              , BorshMaxSize g
              , BorshMaxSize h
              , BorshMaxSize i
              , BorshMaxSize j
              )
           => BorshMaxSize (a, b, c, d, e, f, g, h, i, j)
deriving via AsStruct (a, b, c, d, e, f, g, h, i, j)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              , ToBorsh g
              , ToBorsh h
              , ToBorsh i
              , ToBorsh j
              )
           => ToBorsh (a, b, c, d, e, f, g, h, i, j)
deriving via AsStruct (a, b, c, d, e, f, g, h, i, j)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              , FromBorsh f
              , FromBorsh g
              , FromBorsh h
              , FromBorsh i
              , FromBorsh j
              )
           => FromBorsh (a, b, c, d, e, f, g, h, i, j)

{-------------------------------------------------------------------------------
  Instances for other common Haskell types
-------------------------------------------------------------------------------}

-- Lazy ByteString

instance BorshSize L.ByteString where
  type StaticBorshSize L.ByteString = 'HasVariableSize
  borshSize _ = SizeVariable

instance ToBorsh L.ByteString where
  encodeBorsh = encodeLazyByteString

instance FromBorsh L.ByteString where
  decodeBorsh = decodeLazyByteString

-- Strict ByteString

instance BorshSize S.ByteString where
  type StaticBorshSize S.ByteString = 'HasVariableSize
  borshSize _ = SizeVariable

instance ToBorsh S.ByteString where
  encodeBorsh = encodeStrictByteString

instance FromBorsh S.ByteString where
  decodeBorsh = decodeStrictByteString

-- Char

instance BorshSize Char where
  type StaticBorshSize Char = 'HasKnownSize
  borshSize _ = SizeKnown 4

deriving via KnownImpliesMax Char instance BorshMaxSize Char

instance ToBorsh Char where
  encodeBorsh = encodeChar

instance FromBorsh Char where
  decodeBorsh = decodeChar

-- Bool

instance BorshSize Bool where
  type StaticBorshSize Bool = 'HasKnownSize
  borshSize _ = SizeKnown 1

deriving via KnownImpliesMax Bool instance BorshMaxSize Bool

instance ToBorsh Bool where
  encodeBorsh = encodeBool

instance FromBorsh Bool where
  decodeBorsh = decodeBool

-- Either

deriving
  via AsEnum (Either a b)
  instance BorshSize (Either a b)

deriving
  via AsEnum (Either a b)
  instance (BorshMaxSize a, BorshMaxSize b) => BorshMaxSize (Either a b)

deriving
  via AsEnum (Either a b)
  instance (ToBorsh a, ToBorsh b) => ToBorsh (Either a b)

deriving
  via AsEnum (Either a b)
  instance (FromBorsh a, FromBorsh b) => FromBorsh (Either a b)

{-------------------------------------------------------------------------------
  Internal auxiliary: size of products and sums-of-products
-------------------------------------------------------------------------------}

-- | A product of types has known size if all types in the products do
type family ProdKnownSize (xs :: [Type]) :: KnownSize where
  ProdKnownSize '[]       = 'HasKnownSize
  ProdKnownSize (x ': xs) = ProdKnownAux (StaticBorshSize x) xs

-- | Auxiliary to 'ProdKnownSize'
--
-- Defined in such a way that we know the result is of variable size as soon
-- as we encounter the first type of variable size (independent of the tail).
type family ProdKnownAux (x :: KnownSize) (xs :: [Type]) :: KnownSize where
  ProdKnownAux 'HasKnownSize    xs = ProdKnownSize xs
  ProdKnownAux 'HasVariableSize xs = 'HasVariableSize

-- | A sum of products has known size if it has at most one constructor,
-- and all arguments of that constructor have known size
type family SumKnownSize (xs :: [[Type]]) :: KnownSize where
  SumKnownSize '[]   = 'HasKnownSize
  SumKnownSize '[xs] = ProdKnownSize xs
  SumKnownSize _     = 'HasVariableSize

-- | Type-level composition of 'Size' and 'StaticBorshSize'
newtype SoK (a :: Type) = SoK (Size (StaticBorshSize a))

constrSoK :: forall a. BorshSize a => SoK a
constrSoK = SoK $ borshSize (Proxy @a)

sizeOfProd :: forall xs. All BorshSize xs => Proxy xs -> Size (ProdKnownSize xs)
sizeOfProd _ =
      go (hcpure (Proxy @BorshSize) constrSoK :: NP SoK xs)
    where
      go :: forall xs'. NP SoK xs' -> Size (ProdKnownSize xs')
      go Nil           = SizeKnown 0
      go (SoK s :* ss) =
          case (s, go ss) of
            (SizeKnown sz , SizeKnown sz') -> SizeKnown (sz + sz')
            (SizeKnown _  , SizeVariable ) -> SizeVariable
            (SizeVariable , _            ) -> SizeVariable

-- | Auxiliary class to @BorshSize@ describing the conditions under which the
-- size of the encoding of a value of a sum-type is known.
class BorshSizeSum (xss :: [[Type]]) where
  borshSizeSum :: Proxy xss -> Size (SumKnownSize xss)

instance BorshSizeSum '[] where
  -- In a way the size of the @Void@ type is meaningless, because there /are/
  -- no elements of @Void@, and hence there /is/ no encoding.
  -- TODO: Should we return undefined here..?
  borshSizeSum _ = SizeKnown 0

instance All BorshSize xs => BorshSizeSum '[xs] where
  borshSizeSum _ =
    -- This assumes the presence of the constructor tag
    -- (see detailed discussion in 'AsStruct')
    case sizeOfProd (Proxy @xs) of
      SizeKnown sz -> SizeKnown (sz + 1)
      SizeVariable -> SizeVariable

instance BorshSizeSum (xs ': ys ': zss) where
  borshSizeSum _ = SizeVariable
