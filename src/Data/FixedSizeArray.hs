-- | Fixed size arrays
--
-- Intended for qualified import
--
-- > import Data.FixedSizeArray (FixedSizeArray)
-- > import qualified Data.FixedSizeArray as FSA
module Data.FixedSizeArray (
    FixedSizeArray  -- opaque
  , MFixedSizeArray -- opaque
  , toArray
  , toMArray
    -- * Construction
  , fromList
  , fromArray
  , fromMArray
  , new
  ) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy
import Data.Vector (Vector, MVector)
import GHC.TypeLits

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Fixed size arrays
--
-- @FixedSizeArray n a@ is the Haskell counter-part to the Rust type @[A; N]@.
--
-- NOTE: For convenience, this is an instance of 'G.Vector', but the invariant
-- that the length of the vector should never change is not currently checked.
newtype FixedSizeArray (n :: Nat) (a :: Type) = FromArray {
      toArray :: Vector a
    }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Functor, Foldable)

instance KnownNat n => Traversable (FixedSizeArray n) where
  traverse f = fmap fromArray . traverse f . toArray

-- | Mutable fixed-size arrays
newtype MFixedSizeArray (n :: Nat) s (a :: Type) = FromMArray {
      toMArray :: MVector s a
    }

type instance G.Mutable (FixedSizeArray n) = MFixedSizeArray n

instance GM.MVector (MFixedSizeArray n) a where
  basicLength      = coerce $ GM.basicLength      @MVector @a
  basicUnsafeSlice = coerce $ GM.basicUnsafeSlice @MVector @a
  basicOverlaps    = coerce $ GM.basicOverlaps    @MVector @a
  basicUnsafeNew   = coerce $ GM.basicUnsafeNew   @MVector @a
  basicInitialize  = coerce $ GM.basicInitialize  @MVector @a
  basicUnsafeRead  = coerce $ GM.basicUnsafeRead  @MVector @a
  basicUnsafeWrite = coerce $ GM.basicUnsafeWrite @MVector @a

instance G.Vector (FixedSizeArray n) a where
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @Vector @a
  basicLength       = coerce $ G.basicLength       @Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @Vector @a

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct 'FixedSizeArray' from list of unknown size
--
-- Throws an exception if the list does not have the right number of elements.
fromList :: forall n a. KnownNat n => [a] -> FixedSizeArray n a
fromList = fromArray . G.fromList

-- | Construct 'FixedSizeArray' from array of unknown size
--
-- Throws an exception if the array does not have the right size.
fromArray :: forall n a. KnownNat n => Vector a -> FixedSizeArray n a
fromArray v
  | G.length v == fromIntegral (natVal (Proxy @n)) = FromArray v
  | otherwise = error $ concat [
        "fromArray: invalid length. "
      , "expected " ++ show (natVal (Proxy @n))
      , ", but got "
      , show $ G.length v
      ]

-- | Construct 'FixedSizeArray' from mutable array of unknown size
--
-- Throws an exception if the array does not have the right size.
fromMArray :: forall n s a. KnownNat n => MVector s a -> MFixedSizeArray n s a
fromMArray v
  | GM.length v == fromIntegral (natVal (Proxy @n)) = FromMArray v
  | otherwise = error $ concat [
        "fromArray: invalid length. "
      , "expected " ++ show (natVal (Proxy @n))
      , ", but got "
      , show $ GM.length v
      ]

-- | Construct new mutable array of the appropriate size
new :: forall m n a.
     (GM.PrimMonad m, KnownNat n)
  => m (MFixedSizeArray n (GM.PrimState m) a)
new = FromMArray <$> GM.new (fromIntegral $ natVal (Proxy @n))

