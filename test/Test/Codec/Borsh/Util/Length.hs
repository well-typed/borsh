module Test.Codec.Borsh.Util.Length (
    Length(..)
  , SomeLength(..)
  ) where

import Data.Maybe (fromJust)
import Data.Proxy
import GHC.TypeLits
import Test.QuickCheck

-- | Like Proxy, but with a more informative show instance
data Length (n :: Nat) = Length

lengthVal :: forall n. KnownNat n => Length n -> Int
lengthVal _ = fromIntegral $ natVal (Proxy @n)

instance KnownNat n => Show (Length n) where
  show = show . lengthVal

data SomeLength where
  SomeLength :: KnownNat n => Length n -> SomeLength

toSomeLength :: SomeNat -> SomeLength
toSomeLength (SomeNat n) = SomeLength (aux n)
  where
    aux :: forall n. Proxy n -> Length n
    aux _ = Length

instance Arbitrary SomeLength where
  arbitrary = toSomeLength . fromJust . someNatVal <$> choose (0, 3)
  shrink (SomeLength n) = aux n
    where
      aux :: forall n. KnownNat n => Length n -> [SomeLength]
      aux _ = map (toSomeLength . fromJust . someNatVal) $
                shrink (natVal (Proxy @n))

