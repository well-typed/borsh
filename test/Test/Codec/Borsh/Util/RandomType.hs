-- 11 iterations needed for GHC 9.2.2
{-# OPTIONS_GHC -fconstraint-solver-iterations=11 #-}

module Test.Codec.Borsh.Util.RandomType (
    -- * Types
    BorshType(..)
  , SomeBorshType(..)
  , borshTypeMaxSize

    -- * Values of those types
  , SomeBorshValue(..)
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.FixedSizeArray (FixedSizeArray)
import Data.Foldable (toList)
import Data.Int
import Data.Kind
import Data.Map (Map)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Profunctor
import Data.Set (Set)
import Data.Text (Text)
import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import Generics.SOP
import Generics.SOP.Dict
import Generics.SOP.NP (map_NP)
import GHC.Float
import GHC.TypeLits
import Test.QuickCheck hiding (shrinkIntegral)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.UnorderedContainers ()

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import Codec.Borsh

import qualified Data.FixedSizeArray as FSA

import Test.Codec.Borsh.ExampleType.BTree
import Test.Codec.Borsh.ExampleType.NTree
import Test.Codec.Borsh.ExampleType.SimpleList
import Test.Codec.Borsh.ExampleType.SimpleStructs
import Test.Codec.Borsh.Util.Length
import Test.Codec.Borsh.Util.Orphans ()
import Test.Codec.Borsh.Util.SOP

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

class ( Arbitrary a
      , Show      a
      , Eq        a
      , Ord       a
      , BorshSize a
      , ToBorsh   a
      , FromBorsh a
      ) => CanTest a

instance ( Arbitrary a
         , Show      a
         , Eq        a
         , Ord       a
         , BorshSize a
         , ToBorsh   a
         , FromBorsh a
         ) => CanTest a

{-------------------------------------------------------------------------------
  Auxiliary: working with n-ary products

  We would like to generate arbitrary structs/enums for our property tests. In
  order to do this properly, we need to generate arbitrary-length n-ary products
-------------------------------------------------------------------------------}

data SomeBorshNP where
  SomeBorshNP ::
       ( All CanTest xs
       , All (Compose Show BorshType) xs
       )
    => NP BorshType xs -> SomeBorshNP

deriving instance Show SomeBorshNP

data SomeBorshPOP where
  SomeBorshPOP ::
      ( All2 CanTest (xs ': xss)
      , All (Compose Show (NP BorshType)) (xs ': xss)
      , BorshSizeSum (xs ': xss)
      )
    => POP BorshType (xs ': xss) -> SomeBorshPOP

deriving instance Show SomeBorshPOP

someBorshPOP :: forall xs xss.
    ( All2 CanTest (xs ': xss)
    , All (Compose Show (NP BorshType)) (xs ': xss)
    )
  => POP BorshType (xs ': xss) -> SomeBorshPOP
someBorshPOP xss =
    case dictBorshSizeSum (Proxy @(xs ': xss)) of
      Dict -> SomeBorshPOP xss

generateSomeNP :: Int -> Gen SomeBorshNP
generateSomeNP = \sz -> do
    len <- choose (0, 3) :: Gen Int
    go len (sz `div` (len + 1))
  where
    go :: Int -> Int -> Gen SomeBorshNP
    go i sz | i <= 0    = pure (SomeBorshNP Nil)
            | otherwise = do
                SomeType x <- resize sz arbitraryType
                SomeBorshNP xs  <- go (i - 1) sz
                pure $ SomeBorshNP (x :* xs)

generateSomePOP :: Int -> Gen SomeBorshPOP
generateSomePOP = \sz -> do
    len <- choose (1, 3) :: Gen Int
    go len (sz `div` len)
  where
    go :: Int -> Int -> Gen SomeBorshPOP
    go i sz | i <= 1    = do
                SomeBorshNP np <- generateSomeNP sz
                pure $ someBorshPOP (POP (np :* Nil))
            | otherwise = do
                SomeBorshNP np <- generateSomeNP sz
                SomeBorshPOP (POP nps) <- go (i - 1) sz
                pure $ SomeBorshPOP (POP (np :* nps))

{-------------------------------------------------------------------------------
  Auxiliary: shrunk NPs and POPs
-------------------------------------------------------------------------------}

-- | Shrunk product
--
-- When we shrink products recursively, we need to know that we only shrink
-- products to products: this allows us to prepend something to the product
-- in the recursive case.
data ShrunkNP xs where
  ShrunkNP ::
       ( All CanTest xs'
       , SListI xs'
       )
    => NP BorshType xs' -> ShrinkFun (NP I xs) (NP I xs') -> ShrunkNP xs

fromShrunkNP :: ShrunkNP xs -> SomeShrunkType (NP I xs)
fromShrunkNP (ShrunkNP ts f) =
    case allTestingConstraints ts of
      Dict -> SomeShrunk (BtStruct ts) f

shrinkNpDropHead ::
     All CanTest xs
  => NP BorshType xs -> ShrunkNP (x : xs)
shrinkNpDropHead xs = ShrunkNP xs $ ShrinkFun (return . tl)

shrinkNpShrinkHead ::
     All CanTest (x ': xs)
  => SomeShrunkType x -> NP BorshType xs -> ShrunkNP (x ': xs)
shrinkNpShrinkHead (SomeShrunk x (ShrinkFun f)) xs =
    ShrunkNP (x :* xs) (ShrinkFun $ mapHeadNP (map I . f . unI))

shrinkNpTail ::
     All CanTest (x ': xs)
  => BorshType x -> ShrunkNP xs -> ShrunkNP (x : xs)
shrinkNpTail x (ShrunkNP xs (ShrinkFun f)) =
    ShrunkNP (x :* xs) (ShrinkFun $ mapTailNP f)

-- | Shrink one of the types in the product
shrinkProdElem ::
     All CanTest xs
  => NP BorshType xs -> NP ([] :.: SomeShrunkType) xs -> [ShrunkNP xs]
shrinkProdElem Nil       Nil                  = []
shrinkProdElem (x :* xs) (Comp shrinkA :* ss) = concat [
      map (`shrinkNpShrinkHead` xs) shrinkA
    , map (shrinkNpTail x) $ shrinkProdElem xs ss
    ]

-- | Drop one of the types in the product
shrinkProdSize ::
     All CanTest xs
  => NP BorshType xs -> [ShrunkNP xs]
shrinkProdSize Nil       = []
shrinkProdSize (x :* xs) = shrinkNpDropHead xs
                         : map (shrinkNpTail x) (shrinkProdSize xs)

data ShrunkPOP xss where
  ShrunkPOP ::
       ( All2 CanTest xss'
       , SListI xss'
       , BorshSizeSum xss'
       )
    => POP BorshType xss' -> ShrinkFun (SOP I xss) (SOP I xss') -> ShrunkPOP xss

shrunkPOP :: forall xss xss'.
     All2 CanTest xss'
  => POP BorshType xss'
  -> ShrinkFun (SOP I xss) (SOP I xss')
  -> ShrunkPOP xss
shrunkPOP xss f =
    case dictBorshSizeSum (Proxy @xss') of
      Dict -> ShrunkPOP xss f

fromShrunkPOP :: ShrunkPOP xss -> Maybe (SomeShrunkType (SOP I xss))
fromShrunkPOP (ShrunkPOP ts f) =
    case ts of
      POP Nil      -> Nothing
      POP (_ :* _) ->
        case all2TestingConstraints ts of
          Dict -> Just $ SomeShrunk (BtEnum ts) f

shrinkPOPDropHead ::
     All2 CanTest xss
  => POP BorshType xss -> ShrunkPOP (xs : xss)
shrinkPOPDropHead xss = shrunkPOP xss $ ShrinkFun $ \(SOP x) -> case x of
    Z _  -> []
    S x' -> [SOP x']

shrinkPOPShrinkHead :: forall xs xss.
     All2 CanTest (xs ': xss)
  => ShrunkNP xs -> POP BorshType xss -> ShrunkPOP (xs ': xss)
shrinkPOPShrinkHead (ShrunkNP ts (ShrinkFun f)) (POP xs) =
    shrunkPOP (POP $ ts :* xs) (ShrinkFun $ mapHeadSOP f)

shrinkPOPTail :: forall xs xss.
     All2 CanTest (xs ': xss)
  => NP BorshType xs -> ShrunkPOP xss -> ShrunkPOP (xs : xss)
shrinkPOPTail xs (ShrunkPOP (POP xss) (ShrinkFun f)) =
    shrunkPOP (POP $ xs :* xss) (ShrinkFun $ mapTailSOP f)

-- | Shrink one of the products in a POP
shrinkPOPElem ::
     All2 CanTest xss
  => POP BorshType xss -> NP ([] :.: ShrunkNP) xss -> [ShrunkPOP xss]
shrinkPOPElem (POP Nil)       Nil                  = []
shrinkPOPElem (POP (x :* xs)) (Comp shrinkP :* ss) = concat [
      map (`shrinkPOPShrinkHead` POP xs) shrinkP
    , map (shrinkPOPTail x) $ shrinkPOPElem (POP xs) ss
    ]

shrinkPOPSize ::
     All2 CanTest xss
  => POP BorshType xss -> [ShrunkPOP xss]
shrinkPOPSize (POP Nil) = []
shrinkPOPSize (POP (xs :* xss)) = shrinkPOPDropHead (POP xss)
                                : map (shrinkPOPTail xs) (shrinkPOPSize (POP xss))

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Non-composite types
data BorshSimpleType :: Type -> Type where
  BtU8    :: BorshSimpleType Word8
  BtU16   :: BorshSimpleType Word16
  BtU32   :: BorshSimpleType Word32
  BtU64   :: BorshSimpleType Word64
  BtU128  :: BorshSimpleType Word128
  BtI8    :: BorshSimpleType Int8
  BtI16   :: BorshSimpleType Int16
  BtI32   :: BorshSimpleType Int32
  BtI64   :: BorshSimpleType Int64
  BtI128  :: BorshSimpleType Int128
  BtF32   :: BorshSimpleType Float
  BtF64   :: BorshSimpleType Double
  BtText  :: BorshSimpleType Text
  BtUnit  :: BorshSimpleType ()

  -- Example user-defined types
  BtSimpleList    :: BorshSimpleType SimpleList
  BtSimpleStruct1 :: BorshSimpleType SimpleStruct1
  BtSimpleStruct2 :: BorshSimpleType SimpleStruct2

  -- Common Haskell types
  BtByteString    :: BorshSimpleType ByteString
  BtChar          :: BorshSimpleType Char
  BtBool          :: BorshSimpleType Bool

deriving instance Show (BorshSimpleType a)

data BorshType :: Type -> Type where
  BtSimple :: BorshSimpleType t -> BorshType t

  -- Composite

  BtArray ::
       (KnownNat n, CanTest a)
    => Length n
    -> BorshType a
    -> BorshType (FixedSizeArray n a)

  BtVec ::
       CanTest a
    => BorshType a
    -> BorshType [a]

  BtOption ::
       CanTest a
    => BorshType a
    -> BorshType (Maybe a)

  BtHashSet ::
       (CanTest a)
    => BorshType a
    -> BorshType (Set a)

  BtHashMap ::
       (CanTest k, CanTest a)
    => BorshType k
    -> BorshType a
    -> BorshType (Map k a)

  BtStruct ::
       ( All CanTest xs
       , All (Compose Show BorshType) xs
       )
    => NP BorshType xs
    -> BorshType (NP I xs)

  BtEnum ::
       ( All2 CanTest (xs ': xss)
       , All (Compose Show (NP BorshType)) (xs ': xss)
       , All SListI (xs ': xss)
       )
    => -- POP: For every constructor, and every argument to every constructor,
       -- what is the type of that argument?
       POP BorshType (xs ': xss)
       -- /Values/ are using a /specific/ constructor, hence SOP, not POP
    -> BorshType (SOP I (xs ': xss))

  -- Example user-defined types

  BtBTree ::
       CanTest a
    => BorshType a -> BorshType (BTree a)

  BtNTree ::
       CanTest a
    => BorshType a -> BorshType (NTree a)

  BtPolyStruct :: CanTest a => BorshType a -> BorshType (PolyStruct a)

  -- Common Haskell types
  BtEither ::
       ( CanTest a
       , CanTest b
       )
    => BorshType a -> BorshType b -> BorshType (Either a b)

deriving instance Show (BorshType a)

data SomeBorshType where
  SomeType :: CanTest a => BorshType a -> SomeBorshType

deriving instance Show SomeBorshType

{-------------------------------------------------------------------------------
  Maximum size
-------------------------------------------------------------------------------}

borshTypeMaxSize :: BorshType a -> Maybe (Dict BorshMaxSize a)
borshTypeMaxSize = \case
    BtSimple bt -> borshSimpleTypeMaxSize bt
    BtArray l bt -> do
      Dict <- borshTypeMaxSize bt
      return Dict
    BtVec _ -> Nothing
    BtOption bt -> do
      Dict <- borshTypeMaxSize bt
      return Dict
    BtHashSet _ -> Nothing
    BtHashMap _ _ -> Nothing
    BtStruct xs -> do
      Dict <- fmap all_NP . hsequence' $ hmap (Comp . borshTypeMaxSize) xs
      return Dict
    BtEnum xss -> do
      Dict <- fmap all_POP . hsequence' $ hmap (Comp . borshTypeMaxSize) xss
      return Dict
    BtBTree _ -> Nothing
    BtNTree _ -> Nothing
    BtPolyStruct bt -> do
      Dict <- borshTypeMaxSize bt
      return Dict
    BtEither bt1 bt2 -> do
      Dict <- borshTypeMaxSize bt1
      Dict <- borshTypeMaxSize bt2
      return Dict

borshSimpleTypeMaxSize :: BorshSimpleType a -> Maybe (Dict BorshMaxSize a)
borshSimpleTypeMaxSize = \case
    BtU8            -> Just Dict
    BtU16           -> Just Dict
    BtU32           -> Just Dict
    BtU64           -> Just Dict
    BtU128          -> Just Dict
    BtI8            -> Just Dict
    BtI16           -> Just Dict
    BtI32           -> Just Dict
    BtI64           -> Just Dict
    BtI128          -> Just Dict
    BtF32           -> Just Dict
    BtF64           -> Just Dict
    BtText          -> Nothing
    BtUnit          -> Just Dict
    BtSimpleList    -> Nothing
    BtSimpleStruct1 -> Just Dict
    BtSimpleStruct2 -> Just Dict
    BtByteString    -> Nothing
    BtChar          -> Just Dict
    BtBool          -> Just Dict

{-------------------------------------------------------------------------------
  Shrinking types
-------------------------------------------------------------------------------}

shrinkToUnit :: ShrinkFun a ()
shrinkToUnit = ShrinkFun $ \_ -> return ()

shrinkIntegral :: (Integral a, Num b) => ShrinkFun a b
shrinkIntegral = ShrinkFun $ return . fromIntegral

shrinkU128 :: ShrinkFun Word128 Word64
shrinkU128 = ShrinkFun $ return . word128Lo64

shrinkDouble :: ShrinkFun Double Word64
shrinkDouble = ShrinkFun $ return . castDoubleToWord64

shrinkFloat :: ShrinkFun Float Word32
shrinkFloat = ShrinkFun $ return . castFloatToWord32

shrinkFoldable :: Foldable f => ShrinkFun (f a) a
shrinkFoldable = ShrinkFun toList

shrinkTraversable :: Traversable f => ShrinkFun a b -> ShrinkFun (f a) (f b)
shrinkTraversable (ShrinkFun f) = ShrinkFun $ traverse f

shrinkFixedArraySize :: forall n m a.
      (KnownNat n, KnownNat m)
  => ShrinkFun (FixedSizeArray n a) (FixedSizeArray m a)
shrinkFixedArraySize
  | natVal (Proxy @m) < natVal (Proxy @n)
  = ShrinkFun $
      return . FSA.fromList . take (fromIntegral $ natVal (Proxy @m)) . toList

  | otherwise
  = ShrinkFun $ const []

shrinkHashSet ::
     Ord b
  => ShrinkFun a b
  -> ShrinkFun (Set a) (Set b)
shrinkHashSet = dimap Set.toList Set.fromList . shrinkTraversable

shrinkHashMap ::
     Ord k2
  => ShrinkFun (k1,a) (k2,b)
  -> ShrinkFun (Map k1 a) (Map k2 b)
shrinkHashMap = dimap Map.toList Map.fromList . shrinkTraversable

shrinkHashMapToKey :: ShrinkFun (Map k a) k
shrinkHashMapToKey = ShrinkFun Map.keys

deriving instance (Show a) => Show (SomeShrunkType a)

instance Show (ShrinkFun a b) where
  show _ = "<ShrinkFun>"

newtype ShrinkFun a b = ShrinkFun (a -> [b])

instance Profunctor ShrinkFun where
  dimap f g (ShrinkFun h) = ShrinkFun $ fmap g . h . f

instance Strong ShrinkFun where
  first' (ShrinkFun f) = ShrinkFun $ \(a,c) -> (,c) <$> f a

data SomeShrunkType a where
  SomeShrunk ::
      CanTest b
   => BorshType b
   -> ShrinkFun a b
   -> SomeShrunkType a

-- | Shrink simple type
--
-- See additional discussion in 'shrinkType'
shrinkSimpleType :: BorshSimpleType a -> Maybe (SomeShrunkType a)
shrinkSimpleType = \case
    BtU8    -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtU16   -> Just $ SomeShrunk (BtSimple BtU8)   shrinkIntegral
    BtU32   -> Just $ SomeShrunk (BtSimple BtU16)  shrinkIntegral
    BtU64   -> Just $ SomeShrunk (BtSimple BtU32)  shrinkIntegral
    BtU128  -> Just $ SomeShrunk (BtSimple BtU64)  shrinkU128
    BtI8    -> Just $ SomeShrunk (BtSimple BtU8)   shrinkIntegral
    BtI16   -> Just $ SomeShrunk (BtSimple BtU16)  shrinkIntegral
    BtI32   -> Just $ SomeShrunk (BtSimple BtU32)  shrinkIntegral
    BtI64   -> Just $ SomeShrunk (BtSimple BtU64)  shrinkIntegral
    BtI128  -> Just $ SomeShrunk (BtSimple BtU128) shrinkIntegral
    BtF32   -> Just $ SomeShrunk (BtSimple BtU32)  shrinkFloat
    BtF64   -> Just $ SomeShrunk (BtSimple BtU64)  shrinkDouble
    BtText  -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtUnit  -> Nothing

    -- TODO: We /could/ shrink these to one of their fields (not very important)
    BtSimpleList    -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtSimpleStruct1 -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtSimpleStruct2 -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtByteString    -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit

    BtChar -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit
    BtBool -> Just $ SomeShrunk (BtSimple BtUnit) shrinkToUnit

-- | Shrink type
--
-- * We do not try to shrink quickly; one step at a time is fine
-- * We shrink unsigned sized types to the smaller versions (@Word64@ -> @Word32@)
-- * We shrink signed types to their unsigned version (bit-for-bit)
-- * We shrink floats to words (bit-for-bit)
-- * We can always shrink to unit
--
-- Implementation note: no catch-all case, so that we are forced to consider how
-- to shrink any new types that we add.
shrinkType :: BorshType a -> [SomeShrunkType a]
shrinkType = \case

    -- Non-composite

    BtSimple t -> maybeToList $ shrinkSimpleType t

    -- Composite

    BtArray n t -> concat [
          -- Drop the array altogether
          [ SomeShrunk t shrinkFoldable ]

          -- Shrink the size of the array
        , [ SomeShrunk (BtArray n' t) shrinkFixedArraySize
          | SomeLength n' <- shrink (SomeLength n)
          ]

          -- Shrink the type of the elements
        , [ SomeShrunk (BtArray n t') (shrinkTraversable f)
          | SomeShrunk t' f <- shrinkType t
          ]
        ]

    BtVec t -> concat [
          [ SomeShrunk t shrinkFoldable ]
        , [ SomeShrunk (BtVec t') (shrinkTraversable f)
          | SomeShrunk t' f <- shrinkType t]
        ]

    BtOption t -> concat [
          [ SomeShrunk t shrinkFoldable ]
        , [ SomeShrunk (BtOption t') (shrinkTraversable f)
          | SomeShrunk t' f <- shrinkType t
          ]
        ]

    BtHashSet t -> concat [
          [ SomeShrunk t shrinkFoldable ]
        , [ SomeShrunk (BtHashSet t') (shrinkHashSet f)
          | SomeShrunk t' f <- shrinkType t
          ]
        ]

    BtHashMap k t -> concat [
          [ SomeShrunk t shrinkFoldable ]
        , [ SomeShrunk k shrinkHashMapToKey ]
        , [ SomeShrunk (BtHashMap k  t') (shrinkHashMap (second' f))
          | SomeShrunk t' f <- shrinkType t
          ]
        , [ SomeShrunk (BtHashMap k' t ) (shrinkHashMap (first'  f))
          | SomeShrunk k' f <- shrinkType k
          ]
        ]

    BtStruct xs  -> map fromShrunkNP (shrinkNP xs)

    BtEnum xss -> mapMaybe fromShrunkPOP (shrinkPOP xss)

    -- User-defined

    BtBTree t -> concat [
          [ SomeShrunk t $ ShrinkFun $ \case
              BTip      -> []
              BLeaf a   -> [a]
              BNode _ _ -> []
          ]

        , [ SomeShrunk (BtBTree t') (ShrinkFun $ traverse f)
          | SomeShrunk t' (ShrinkFun f) <- shrinkType t
          ]
        ]

    BtNTree t -> concat [
          [ SomeShrunk t $ ShrinkFun $ \case
              NLeaf     -> []
              NNode a _ -> [a]
          ]

        , [ SomeShrunk (BtNTree t') (ShrinkFun $ traverse f)
          | SomeShrunk t' (ShrinkFun f) <- shrinkType t
          ]
        ]

    BtPolyStruct t -> concat [
          [ SomeShrunk t $ ShrinkFun $ \case
              Poly x _ _ -> [ x ]
          ]

        , [ SomeShrunk (BtPolyStruct t')
              ( ShrinkFun $ \(Poly x y z) ->
                  Poly <$> (f x) <*> (f y) <*> (f z)
              )
          | SomeShrunk t' (ShrinkFun f) <- shrinkType t
          ]
        ]

    BtEither ta tb -> concat [
          [ SomeShrunk ta $ ShrinkFun $ \case
              Left  a -> [ a ]
              Right _ -> []
          ]

        , [ SomeShrunk tb $ ShrinkFun $ \case
              Left  _ -> []
              Right b -> [ b ]
          ]

        , [ SomeShrunk (BtEither ta' tb')
              ( ShrinkFun $ \case
                  Left  a -> Left  <$> fa a
                  Right b -> Right <$> fb b
              )
          | SomeShrunk ta' (ShrinkFun fa) <- shrinkType ta
          , SomeShrunk tb' (ShrinkFun fb) <- shrinkType tb
          ]
        ]

shrinkNP :: All CanTest xs => NP BorshType xs -> [ShrunkNP xs]
shrinkNP xs = case allTestingConstraints xs of
  Dict ->
    concat [
        -- Shrink one of the types in the product
        shrinkProdElem xs (map_NP (Comp . shrinkType) xs)

        -- Drop one of the types in the product
      , shrinkProdSize xs
      ]

shrinkPOP ::
     All2 CanTest xss
  => POP BorshType xss -> [ShrunkPOP xss]
shrinkPOP xss =
    concat [
        -- Shrink one of the products in the product
        shrinkPOPElem xss (
            hcmap p (Comp . shrinkNP) (unPOP xss)
          )

        -- Drop one of the products in the product
      , shrinkPOPSize xss
      ]
  where
    p = Proxy :: Proxy (All CanTest)

{-------------------------------------------------------------------------------
  Arbitrary instance for BorshType
-------------------------------------------------------------------------------}

arbitrarySimpleType :: Gen SomeBorshType
arbitrarySimpleType = elements [
      SomeType $ BtSimple BtU8
    , SomeType $ BtSimple BtU16
    , SomeType $ BtSimple BtU32
    , SomeType $ BtSimple BtU64
    , SomeType $ BtSimple BtU128
    , SomeType $ BtSimple BtI8
    , SomeType $ BtSimple BtI16
    , SomeType $ BtSimple BtI32
    , SomeType $ BtSimple BtI64
    , SomeType $ BtSimple BtI128
    , SomeType $ BtSimple BtF32
    , SomeType $ BtSimple BtF64
    , SomeType $ BtSimple BtUnit
    , SomeType $ BtSimple BtText
    , SomeType $ BtSimple BtSimpleList
    , SomeType $ BtSimple BtSimpleStruct1
    , SomeType $ BtSimple BtSimpleStruct2
    , SomeType $ BtSimple BtByteString
    , SomeType $ BtSimple BtChar
    , SomeType $ BtSimple BtBool
    ]
  where
    _coveredAllCases :: BorshSimpleType typ -> ()
    _coveredAllCases BtU8            = ()
    _coveredAllCases BtU16           = ()
    _coveredAllCases BtU32           = ()
    _coveredAllCases BtU64           = ()
    _coveredAllCases BtU128          = ()
    _coveredAllCases BtI8            = ()
    _coveredAllCases BtI16           = ()
    _coveredAllCases BtI32           = ()
    _coveredAllCases BtI64           = ()
    _coveredAllCases BtI128          = ()
    _coveredAllCases BtF32           = ()
    _coveredAllCases BtF64           = ()
    _coveredAllCases BtUnit          = ()
    _coveredAllCases BtText          = ()
    _coveredAllCases BtSimpleList    = ()
    _coveredAllCases BtSimpleStruct1 = ()
    _coveredAllCases BtSimpleStruct2 = ()
    _coveredAllCases BtByteString    = ()
    _coveredAllCases BtChar          = ()
    _coveredAllCases BtBool          = ()

-- | Generate arbitrary type
--
-- We have to be careful here: we are generating a recursive structure, and so
-- are susceptible to <https://en.wikipedia.org/wiki/St._Petersburg_paradox>.
-- We need to keep track of the number of elements we want to generate.
arbitraryType :: Gen SomeBorshType
arbitraryType = sized go
  where
    go :: Int -> Gen SomeBorshType
    go sz
      | sz <= 0   = arbitrarySimpleType
      | otherwise = oneof [
          arbitrarySimpleType

          -- Composite

        ,     (\(SomeLength n) (SomeType t) -> SomeType (BtArray n t))
          <$> arbitrary
          <*> go (sz - 1)

        ,     (\(SomeType t) -> SomeType (BtVec t))
          <$> go (sz - 1)

        ,     (\(SomeType t) -> SomeType (BtOption t))
          <$> go (sz - 1)

        ,     (\(SomeType t) -> SomeType (BtHashSet t))
          <$> go (sz - 1)

        ,     (\(SomeType k) (SomeType t) -> SomeType (BtHashMap k t))
          <$> go (sz `div` 2)
          <*> go (sz `div` 2)

        ,     (\(SomeBorshNP xs) ->
                  case allTestingConstraints xs of
                    Dict -> SomeType (BtStruct xs)
                )
          <$> generateSomeNP sz

        ,     (\(SomeBorshPOP xss) ->
                  case all2TestingConstraints xss of
                    Dict -> SomeType (BtEnum xss)
                )
          <$> generateSomePOP sz

          -- Example user-defined types

        , (\(SomeType t) -> SomeType (BtBTree t)) <$> go (sz - 1)

        , (\(SomeType t) -> SomeType (BtNTree t)) <$> go (sz - 1)

        ,     (\(SomeType t) -> SomeType (BtPolyStruct t))
          <$> go (sz - 1)

          -- Common Haskell types

        ,     (\(SomeType ta) (SomeType tb) -> SomeType (BtEither ta tb))
          <$> go (sz `div` 2)
          <*> go (sz `div` 2)

        ]

    _coveredAllCases :: BorshType typ -> ()
    _coveredAllCases BtSimple{}     = ()
    _coveredAllCases BtArray{}      = ()
    _coveredAllCases BtVec{}        = ()
    _coveredAllCases BtOption{}     = ()
    _coveredAllCases BtHashSet{}    = ()
    _coveredAllCases BtHashMap{}    = ()
    _coveredAllCases BtStruct{}     = ()
    _coveredAllCases BtEnum{}       = ()
    _coveredAllCases BtBTree{}      = ()
    _coveredAllCases BtNTree{}      = ()
    _coveredAllCases BtPolyStruct{} = ()
    _coveredAllCases BtEither{}     = ()

instance Arbitrary SomeBorshType where
  arbitrary = arbitraryType
  shrink (SomeType typ) = [SomeType typ' | SomeShrunk typ' _ <- shrinkType typ]

{-------------------------------------------------------------------------------
  Values of those types
-------------------------------------------------------------------------------}

data SomeBorshValue where
  SomeValue :: CanTest a => BorshType a -> a -> SomeBorshValue

deriving instance Show SomeBorshValue

-- | Generate arbitrary value
--
-- We have to be careful here: we are generating a recursive structure, and so
-- are susceptible to <https://en.wikipedia.org/wiki/St._Petersburg_paradox>.
-- We need to keep track of the number of elements we want to generate.
arbitraryValue :: BorshType a -> Gen a
arbitraryValue = \t -> sized $ \sz ->
    go True sz t
  where
    goSimple :: Bool -> BorshSimpleType a -> Gen a
    goSimple topLevel = \case
        BtU8   -> arbitrary
        BtU16  -> arbitrary
        BtU32  -> arbitrary
        BtU64  -> arbitrary
        BtU128 -> arbitrary
        BtI8   -> arbitrary
        BtI16  -> arbitrary
        BtI32  -> arbitrary
        BtI64  -> arbitrary
        BtI128 -> arbitrary
        BtF32  -> arbitrary
        BtF64  -> arbitrary
        BtUnit -> arbitrary
        BtText -> sized $ \sz -> do
                          numChars <- choose (0, sz)
                          Text.pack <$> replicateM numChars arbitrary

        BtSimpleList ->
          if topLevel then
            arbitraryLargeSimpleList
          else sized $ \sz -> do
            n <- choose (0, sz)
            arbitrarySimpleListOfSize n

        BtSimpleStruct1 ->
              Struct1
          <$> goSimple False BtU8
          <*> goSimple False BtUnit
          <*> goSimple False BtU64

        BtSimpleStruct2 ->
              Struct2
          <$> goSimple False BtUnit
          <*> goSimple False BtSimpleStruct1
          <*> goSimple False BtU16

        BtByteString -> arbitrary
        BtChar       -> arbitrary
        BtBool       -> arbitrary

    go :: Bool -> Int -> BorshType a -> Gen a
    go topLevel sz | sz < 0 = go topLevel 0
    go topLevel sz = \case
        -- Non-composite

        BtSimple t -> goSimple topLevel t

        -- Composite

        BtArray tn t ->
            fmap FSA.fromList $
              replicateM n $ go False (sz `div` (n + 1)) t
          where
            n :: Int
            n = fromIntegral $ natVal tn

        BtVec t -> do
          n <- choose (0, sz)
          replicateM n $ go False (sz `div` (n + 1)) t

        BtOption t -> oneof [
              return Nothing
            , Just <$> go False (sz - 1) t
            ]

        BtHashSet t -> Set.fromList <$> do
          n <- choose (0, sz)
          replicateM n $ go False (sz `div` (n + 1)) t

        BtHashMap k t-> Map.fromList <$> do
          n <- choose (0, sz)
          replicateM n $ (,)
            <$> go False (sz `div` (n + 1) `div` 2) k
            <*> go False (sz `div` (n + 1) `div` 2) t

        BtStruct ts ->
          case allTestingConstraints ts of
            Dict -> goNP ts sz

        BtEnum tss ->
          case all2TestingConstraints tss of
            Dict -> goSOP tss sz

        -- Example user-defined types

        BtBTree t -> do
          n  <- choose (0, sz)
          xs <- replicateM n $ go False (sz `div` (n + 1)) t
          arbitraryBTreeWithElems xs

        BtNTree t -> do
          n  <- choose (0, sz)
          xs <- replicateM n $ go False (sz `div` (n + 1)) t
          arbitraryNTreeWithElems xs

        BtPolyStruct t ->
              Poly
          <$> go False (sz `div` 3) t
          <*> go False (sz `div` 3) t
          <*> go False (sz `div` 3) t

        BtEither ta tb -> oneof [
              Left  <$> go False (sz `div` 2) ta
            , Right <$> go False (sz `div` 2) tb
            ]

    -- NP and SOP

    goNP :: SListI xs => NP BorshType xs -> Int -> Gen (NP I xs)
    goNP ts sz =
        arbitraryNP $ hmap (go False (sz `div` (numArgs + 1))) ts
      where
        numArgs :: Int
        numArgs = lengthNP ts

    goSOP :: forall xs xss.
         All SListI (xs ': xss)
      => POP BorshType (xs : xss)
      -> Int
      -> Gen (SOP I (xs : xss))
    goSOP tss sz =
        arbitrarySOP $ hmap (go False (sz `div` (maxNumArgs + 1))) tss
      where
        -- We conservatively divide by the number of arguments of the
        -- constructor with the most arguments
        maxNumArgs :: Int
        maxNumArgs =
            maximum . hcollapse $
              hcmap (Proxy @SListI) (K . lengthNP) (unPOP tss)

instance Arbitrary SomeBorshValue where
  arbitrary = do
      SomeType typ <- arbitrary
      val <- arbitraryValue typ
      return $ SomeValue typ val

  shrink (SomeValue typ val) = concat [
        -- Shrink the type
        [ SomeValue typ' val'
        | SomeShrunk typ' (ShrinkFun f) <- shrinkType typ
        , val' <- f val
        ]

        -- Shrink the value
      , [ SomeValue typ val'
        | val' <- shrink val
        ]
      ]

{-------------------------------------------------------------------------------
  Reasoning
-------------------------------------------------------------------------------}

dictBorshSizeSum :: forall xss proxy.
     All2 CanTest xss
  => proxy xss
  -> Dict BorshSizeSum xss
dictBorshSizeSum _ =
    case all2TestingConstraints (Proxy @xss) of
      Dict -> aux shape
  where
    aux :: All2 BorshSize xss => Shape xss -> Dict BorshSizeSum xss
    aux ShapeNil                  = Dict
    aux (ShapeCons ShapeNil)      = Dict
    aux (ShapeCons (ShapeCons _)) = Dict

allTestingConstraints :: forall proxy xs.
     All CanTest xs
  => proxy xs
  -> Dict (
              All (Compose Show I)
        `And` All (Compose Show BorshType)
        `And` All (Compose Show FieldInfo)
        `And` All (Compose Eq I)
        `And` All (Compose Ord I)
        `And` All Arbitrary
        `And` All BorshSize
        `And` All ToBorsh
        `And` All FromBorsh
      ) xs
allTestingConstraints _ = transformAllConstraints $ unAll_NP dict
  where dict = Dict :: Dict (All CanTest) xs

transformAllConstraints ::
     All CanTest xs
  => NP (Dict CanTest) xs
  -> Dict (
                All (Compose Show I)
          `And` All (Compose Show BorshType)
          `And` All (Compose Show FieldInfo)
          `And` All (Compose Eq I)
          `And` All (Compose Ord I)
          `And` All Arbitrary
          `And` All BorshSize
          `And` All ToBorsh
          `And` All FromBorsh
    ) xs
transformAllConstraints Nil       = Dict
transformAllConstraints (x :* xs) =
  case (x, transformAllConstraints xs) of
    (Dict, Dict) -> Dict

all2TestingConstraints :: forall proxy xss.
     All2 CanTest xss
  => proxy xss
  -> Dict (
              All (Compose Eq (NP I))
        `And` All (Compose Ord (NP I))
        `And` All (Compose Show (NP I))
        `And` All2 (Compose Show I)
        `And` All2 (Compose Eq I)
        `And` All2 (Compose Ord I)
        `And` All2 Arbitrary
        `And` All SListI
        `And` All2 BorshSize
        `And` All2 ToBorsh
        `And` All2 FromBorsh
        `And` All (Compose Show (NP BorshType))
      ) xss
all2TestingConstraints _ = transformAll2Constraints $ unAll_POP dict
  where dict = Dict :: Dict (All2 CanTest) xss

transformAll2Constraints ::
     All2 CanTest xss
  => POP (Dict CanTest) xss
  -> Dict (
              All (Compose Eq (NP I))
        `And` All (Compose Ord (NP I))
        `And` All (Compose Show (NP I))
        `And` All2 (Compose Show I)
        `And` All2 (Compose Eq I)
        `And` All2 (Compose Ord I)
        `And` All2 Arbitrary
        `And` All SListI
        `And` All2 BorshSize
        `And` All2 ToBorsh
        `And` All2 FromBorsh
        `And` All (Compose Show (NP BorshType))
      ) xss
transformAll2Constraints (POP Nil)       = Dict
transformAll2Constraints (POP (x :* xs)) =
  case (transformAllConstraints x, transformAll2Constraints (POP xs)) of
    (Dict, Dict) -> Dict

