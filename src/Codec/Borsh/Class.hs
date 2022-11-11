{-# LANGUAGE PolyKinds #-}

module Codec.Borsh.Class (
    -- * Serialisation
    ToBorsh(..)
  , FromBorsh(..)
    -- ** Deriving-via support
  , Struct(..)
    -- * Size information
  , KnownSize(..)
  , Size(..)
  , BorshSize(..)
  , BorshSizeSum(..)
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
import Data.Int128
import Data.Word128 (Word128)

{-------------------------------------------------------------------------------
  Size information

  We do not try to compute the size at the type-level (we don't need to and
  this would get messy), but we /do/ record at the type level whether the
  size is statically known or not.
-------------------------------------------------------------------------------}

data KnownSize = HasKnownSize | HasVariableSize

-- | The statically known size of encodings of values of a particular type.
data Size (a :: KnownSize) where
  SizeKnown    :: Int -> Size 'HasKnownSize
  SizeVariable :: Size 'HasVariableSize

deriving instance Show (Size a)
deriving instance Eq   (Size a)

class BorshSize (a :: Type) where
  type StaticBorshSize a :: KnownSize
  type StaticBorshSize a = SumKnownSize (Code a)

  -- | Size of the Borsh encoding, if known ahead of time
  --
  -- See 'encodeBorsh' for discussion of the generic instance.
  borshSize :: Proxy a -> Size (StaticBorshSize a)

  default borshSize ::
       ( StaticBorshSize a ~ SumKnownSize (Code a)
       , BorshSizeSum (Code a)
       )
    => Proxy a -> Size (StaticBorshSize a)
  borshSize _ = borshSizeSum (Proxy @(Code a))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class BorshSize a => ToBorsh a where
  -- | Encoder to Borsh
  --
  -- NOTE: The default generic encoder uses the Borsh encoding for enums,
  -- and will therefore use constructor tag; see 'Struct' for detailed
  -- discussion. Since the spec mandates the presence of that constructor tag,
  -- the generic encoder/decoder does not apply to types without constructors.
  encodeBorsh :: Encoder a

  default encodeBorsh ::
       (Generic a, BorshSizeSum (Code a), All2 ToBorsh (Code a))
    => Encoder a
  encodeBorsh = Encoder $ runEncoder encodeBorsh . from

class BorshSize a => FromBorsh a where
  -- | Decode from Borsh
  --
  -- See 'encodeBorsh' for discussion of the generic instance.
  decodeBorsh :: Decoder s a

  default decodeBorsh ::
       (Generic a, BorshSizeSum (Code a), All2 FromBorsh (Code a))
    => Decoder s a
  decodeBorsh = to <$> decodeBorsh

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Deriving-via support for structs
--
-- The Borsh spec <https://borsh.io/> mandates that enums have a tag indicating
-- the constructor, even when there is only a single constructor in the enum.
-- In Rust this makes more sense than in Haskell, since in Rust enums and
-- structs are introduced through different keywords. In Haskell, of course,
-- the only difference between them is that a struct is an enum with a single
-- constructor.
--
-- The default generic encoder en decoder you get in 'ToBorsh' and 'FromBorsh'
-- will therefore add the tag, independent of the number of constructors. If
-- you want the encoding of a struct, without the tag, you need to use deriving
-- via:
--
-- > data MyStruct = ..
-- >   deriving (BorshSize, ToBorsh, FromBorsh) via Struct MyStruct
--
-- NOTE: Doing so may have consequences for forwards compatibility: if a tag
-- is present, additional constructors can be added without invalidating the
-- encoding of existing constructors.
newtype Struct a = Struct { getStruct :: a }

instance (IsProductType a xs, All BorshSize xs) => BorshSize (Struct a) where
  type StaticBorshSize (Struct a) = ProdKnownSize (ProductCode a)
  borshSize _ = sizeOfProd (Proxy @(ProductCode a))

instance ( IsProductType a xs
         , All BorshSize xs
         , All ToBorsh xs
         ) => ToBorsh (Struct a) where
  encodeBorsh = contramap (productTypeFrom . getStruct) encodeBorsh

instance ( IsProductType a xs
         , All BorshSize xs
         , All FromBorsh xs
         ) => FromBorsh (Struct a) where
  decodeBorsh = fmap (Struct . productTypeTo) decodeBorsh

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

instance BorshSize [a] where
  -- Use generic defaults

instance BorshSize (Maybe a) where
  -- Use generic defaults

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

deriving via Struct () instance BorshSize ()
deriving via Struct () instance ToBorsh   ()
deriving via Struct () instance FromBorsh ()

-- size 2

deriving via Struct (a, b)
         instance
              ( BorshSize a
              , BorshSize b
              )
           => BorshSize (a, b)
deriving via Struct (a, b)
         instance
              ( ToBorsh a
              , ToBorsh b
              )
           => ToBorsh (a, b)
deriving via Struct (a, b)
         instance
              ( FromBorsh a
              , FromBorsh b
              )
           => FromBorsh (a, b)

-- size 3

deriving via Struct (a, b, c)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              )
           => BorshSize (a, b, c)
deriving via Struct (a, b, c)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              )
           => ToBorsh (a, b, c)
deriving via Struct (a, b, c)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              )
           => FromBorsh (a, b, c)

-- size 4

deriving via Struct (a, b, c, d)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              )
           => BorshSize (a, b, c, d)
deriving via Struct (a, b, c, d)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              )
           => ToBorsh (a, b, c, d)
deriving via Struct (a, b, c, d)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              )
           => FromBorsh (a, b, c, d)

-- size 5

deriving via Struct (a, b, c, d, e)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              )
           => BorshSize (a, b, c, d, e)
deriving via Struct (a, b, c, d, e)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              )
           => ToBorsh (a, b, c, d, e)
deriving via Struct (a, b, c, d, e)
         instance
              ( FromBorsh a
              , FromBorsh b
              , FromBorsh c
              , FromBorsh d
              , FromBorsh e
              )
           => FromBorsh (a, b, c, d, e)

-- size 6

deriving via Struct (a, b, c, d, e, f)
         instance
              ( BorshSize a
              , BorshSize b
              , BorshSize c
              , BorshSize d
              , BorshSize e
              , BorshSize f
              )
           => BorshSize (a, b, c, d, e, f)
deriving via Struct (a, b, c, d, e, f)
         instance
              ( ToBorsh a
              , ToBorsh b
              , ToBorsh c
              , ToBorsh d
              , ToBorsh e
              , ToBorsh f
              )
           => ToBorsh (a, b, c, d, e, f)
deriving via Struct (a, b, c, d, e, f)
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

deriving via Struct (a, b, c, d, e, f, g)
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
deriving via Struct (a, b, c, d, e, f, g)
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
deriving via Struct (a, b, c, d, e, f, g)
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

deriving via Struct (a, b, c, d, e, f, g, h)
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
deriving via Struct (a, b, c, d, e, f, g, h)
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
deriving via Struct (a, b, c, d, e, f, g, h)
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

deriving via Struct (a, b, c, d, e, f, g, h, i)
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
deriving via Struct (a, b, c, d, e, f, g, h, i)
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
deriving via Struct (a, b, c, d, e, f, g, h, i)
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

deriving via Struct (a, b, c, d, e, f, g, h, i, j)
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
deriving via Struct (a, b, c, d, e, f, g, h, i, j)
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
deriving via Struct (a, b, c, d, e, f, g, h, i, j)
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

instance ToBorsh Char where
  encodeBorsh = encodeChar

instance FromBorsh Char where
  decodeBorsh = decodeChar

-- Bool

instance BorshSize Bool where
  type StaticBorshSize Bool = 'HasKnownSize
  borshSize _ = SizeKnown 1

instance ToBorsh Bool where
  encodeBorsh = encodeBool

instance FromBorsh Bool where
  decodeBorsh = decodeBool

-- Either

deriving instance BorshSize (Either a b)
deriving instance (ToBorsh   a, ToBorsh   b) => ToBorsh   (Either a b)
deriving instance (FromBorsh a, FromBorsh b) => FromBorsh (Either a b)

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
    -- (see detailed discussion in 'Struct')
    case sizeOfProd (Proxy @xs) of
      SizeKnown sz -> SizeKnown (sz + 1)
      SizeVariable -> SizeVariable

instance BorshSizeSum (xs ': ys ': zss) where
  borshSizeSum _ = SizeVariable
