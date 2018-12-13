{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Map.Strict.Invertible
  ( InvertibleMap
  , unInvertibleMap
  , union
  , difference
  , singleton
  , negativeSingleton
  , fromList
  , toList
  )
where

import           Data.Group
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data InvertibleMap k a = InvertibleMap
  { getPositive :: Map k a
  , getNegative :: Map k a
  } deriving (Ord, Generic, Functor, Foldable, Traversable)

instance (Show k, Show a, Ord k) => Show (InvertibleMap k a) where
  show m = show (unInvertibleMap m)

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (InvertibleMap k a) where
  arbitrary = InvertibleMap <$> arbitrary <*> arbitrary

unInvertibleMap :: Ord k => InvertibleMap k a -> Map k a
unInvertibleMap is = getPositive is `Map.difference` getNegative is

instance (Ord k, Eq a) => Eq (InvertibleMap k a) where
  a == b = unInvertibleMap a == unInvertibleMap b

instance Ord k => Semigroup (InvertibleMap k a) where
  a <> b = InvertibleMap
    { getPositive = getPositive a <> getPositive b
    , getNegative = getNegative a <> getNegative b
    }

instance Ord k => Monoid (InvertibleMap k a) where
  mempty = InvertibleMap mempty mempty

instance Ord k => Group (InvertibleMap k a) where
  invert (InvertibleMap p n) = InvertibleMap n p

singleton :: Ord k => k -> a -> InvertibleMap k a
singleton k a = InvertibleMap (Map.singleton k a) mempty

negativeSingleton :: Ord k => k -> a -> InvertibleMap k a
negativeSingleton k a = InvertibleMap mempty (Map.singleton k a)

union :: Ord k => InvertibleMap k a -> InvertibleMap k a -> InvertibleMap k a
union = (<>)

difference
  :: Ord k => InvertibleMap k a -> InvertibleMap k a -> InvertibleMap k a
difference a b = a <> invert b

fromList :: Ord k => [(k, a)] -> InvertibleMap k a
fromList = mconcat . map (uncurry singleton)

toList :: Ord k => InvertibleMap k a -> [(k, a)]
toList = Map.toList . unInvertibleMap
