{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec.Borsh.Util.SOP (
    -- * Mapping
    mapHeadNP
  , mapTailNP
  , mapHeadSOP
  , mapTailSOP
    -- * Misc
  , lengthNP
    -- * Generation
  , arbitraryNP
  , arbitrarySOP
  ) where

import Data.SOP
import Test.QuickCheck hiding (pattern Fn)
import Optics.Core

import Test.Codec.Borsh.Util.QuickCheck

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

mapHeadNP ::
     Functor m
  => (f x -> m (f y))
  -> NP f (x ': xs) -> m (NP f (y ': xs))
mapHeadNP f (x :* xs) = fmap (:* xs) (f x)

mapTailNP ::
     Functor m
  => (NP f xs -> m (NP f ys))
  -> NP f (z : xs) -> m (NP f (z : ys))
mapTailNP f (z :* xs) = fmap (z :*) (f xs)

mapHeadSOP ::
     Applicative m
  => (NP f xs -> m (NP f ys))
  -> SOP f (xs ': xss) -> m (SOP f (ys ': xss))
mapHeadSOP f (SOP (Z x)) = SOP . Z <$> f x
mapHeadSOP _ (SOP (S s)) = pure $ SOP (S s)

mapTailSOP ::
     Applicative m
  => (SOP f xss -> m (SOP f yss))
  -> SOP f (zs : xss) -> m (SOP f (zs : yss))
mapTailSOP _ (SOP (Z x)) = pure $ SOP (Z x)
mapTailSOP f (SOP (S s)) = SOP . S . unSOP <$> f (SOP s)

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

lengthNP :: forall f xs. SListI xs => NP f xs -> Int
lengthNP _ = lengthSList (Proxy @xs)

{-------------------------------------------------------------------------------
  Lenses
-------------------------------------------------------------------------------}

newtype LensNP xs a = LensNP (Lens' (NP I xs) a)

lensHeadNP :: LensNP (x : xs) x
lensHeadNP = LensNP $
    lens
      (unI . hd)
      (\(_ :* xs) x -> I x :* xs)

shiftLensNP :: LensNP xs a -> LensNP (x : xs) a
shiftLensNP (LensNP l) = LensNP $
    lens
      (view l . tl)
      (\(x :* xs) a -> x :* set l a xs)

lensesNP :: forall xs. SListI xs => NP (LensNP xs) xs
lensesNP =
    case sList :: SList xs of
      SNil  -> Nil
      SCons -> lensHeadNP  :* hmap shiftLensNP lensesNP

{-------------------------------------------------------------------------------
  Compositional generation
-------------------------------------------------------------------------------}

arbitraryNP :: SListI xs => NP Gen xs -> Gen (NP I xs)
arbitraryNP = hsequence

-- | Auxiliary to 'arbitrarySOP'
--
-- Post-condition: the result list will have as many entries as the POP.
arbitrarySOP' :: forall xss. All SListI xss => POP Gen xss -> [Gen (SOP I xss)]
arbitrarySOP' =
      hcollapse
    . hczipWith
        (Proxy @SListI) (\(Fn inj) -> K . aux (SOP . unK . inj) )
        (injections @xss @(NP I))
    . unPOP
  where
    aux :: SListI xs => (NP I xs -> SOP I xss) -> NP Gen xs -> Gen (SOP I xss)
    aux inj = fmap inj . arbitraryNP

-- | Generate arbitrary SOP
--
-- The restriction to non-empty SOPs ensures the call to 'oneof' will not fail.
arbitrarySOP ::
     All SListI (xs ': xss)
  => POP Gen (xs ': xss) -> Gen (SOP I (xs ': xss))
arbitrarySOP = oneof . arbitrarySOP'

{-------------------------------------------------------------------------------
  Compositional shrinking
-------------------------------------------------------------------------------}

shrinkNP :: forall xs. SListI xs => NP Shrinker xs -> Shrinker (NP I xs)
shrinkNP =
      mconcat
    . hcollapse
    . hzipWith (\(LensNP l) (Shrinker f) -> K $ Shrinker $ aux l f) lensesNP
  where
    aux :: Lens' (NP I xs) a -> (a -> [a]) -> NP I xs -> [NP I xs]
    aux l f xs = [ set l a' xs | a' <- f (view l xs) ]

shrinkSOP :: forall xss.
     All SListI xss
  => POP Shrinker xss -> Shrinker (SOP I xss)
shrinkSOP = \(POP sss) -> Shrinker $ \(SOP xss) ->
       hcollapse
     $ hczipWith3
         (Proxy @SListI)
         (\ss (Fn inj) xs -> K $ aux (shrinkNP ss) (SOP . unK . inj) xs)
         sss
         (injections @xss @(NP I))
         xss
  where
    aux :: Shrinker (NP I a) -> (NP I a -> SOP I xss) -> NP I a -> [SOP I xss]
    aux (Shrinker f) inj = map inj . f

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance All Arbitrary xs => Arbitrary (NP I xs) where
  arbitrary = arbitraryNP $ hcpure (Proxy @Arbitrary) arbitrary
  shrink    = runShrinker $ shrinkNP $ hcpure (Proxy @Arbitrary) shrinker

instance ( SListI (xs ': xss)
         , All SListI xss
         , All2 Arbitrary (xs ': xss)
         ) => Arbitrary (SOP I (xs ': xss)) where
  arbitrary = arbitrarySOP $ hcpure (Proxy @Arbitrary) arbitrary
  shrink    = runShrinker $ shrinkSOP $ hcpure (Proxy @Arbitrary) shrinker