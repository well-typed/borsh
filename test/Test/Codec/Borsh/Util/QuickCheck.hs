module Test.Codec.Borsh.Util.QuickCheck (
    -- * Compositional shrinking
    Shrinker(..)
  , shrinker
    -- * Generators
  , split2
  , splitN
  ) where

import Control.Monad
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Compositional shrinking
-------------------------------------------------------------------------------}

newtype Shrinker a = Shrinker { runShrinker :: a -> [a] }

instance Semigroup (Shrinker a) where
  Shrinker f <> Shrinker g = Shrinker $ \x -> f x ++ g x

instance Monoid (Shrinker a) where
  mempty = Shrinker $ \_ -> []

shrinker :: Arbitrary a => Shrinker a
shrinker = Shrinker shrink

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

split2 :: [a] -> Gen ([a], [a])
split2 = splitN 2 >=> \case
       [xs,ys] -> return (xs, ys)
       _       -> error "splitN post-condition failed"

-- Post-condition: outer list will contain precisely @n@ elements
splitN :: Int -> [a] -> Gen [[a]]
splitN 0 = const $ pure []
splitN n = shuffle >=> go
  where
    go :: [a] -> Gen [[a]]
    go []     = pure $ replicate n []
    go (x:xs) = do
      splits <- splitN n xs
      select <- choose (0,n-1)
      case splitAt select splits of
        (_  ,[]  )     -> error "expected non-empty tail in split"
        (xs',y:ys) ->
          return $ xs' ++ (x:y):ys

