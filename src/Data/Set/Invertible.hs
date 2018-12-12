{-# LANGUAGE DeriveGeneric #-}
module Data.Set.Invertible
  ( InvertibleSet
  , unInvertibleSet
  , union
  , difference
  , singleton
  , negativeSingleton
  , fromList
  , toList
  )
where

import           Data.Group
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data InvertibleSet a = InvertibleSet
  { getPositive :: Set a
  , getNegative :: Set a
  } deriving (Ord, Generic)

instance (Show a, Ord a) => Show (InvertibleSet a) where
  show s = show (unInvertibleSet s)

instance (Ord a, Arbitrary a) => Arbitrary (InvertibleSet a) where
  arbitrary = InvertibleSet <$> arbitrary <*> arbitrary

unInvertibleSet :: Ord a => InvertibleSet a -> Set a
unInvertibleSet is = getPositive is `Set.difference` getNegative is

instance (Ord a, Eq a) => Eq (InvertibleSet a) where
  a == b = unInvertibleSet a == unInvertibleSet b

instance Ord a => Semigroup (InvertibleSet a) where
  a <> b = InvertibleSet
    { getPositive = getPositive a <> getPositive b
    , getNegative = getNegative a <> getNegative b
    }

instance Ord a => Monoid (InvertibleSet a) where
  mempty = InvertibleSet mempty mempty

instance Ord a => Group (InvertibleSet a) where
  invert (InvertibleSet p n) = InvertibleSet n p

singleton :: Ord a => a -> InvertibleSet a
singleton a = InvertibleSet (Set.singleton a) mempty

negativeSingleton :: Ord a => a -> InvertibleSet a
negativeSingleton a = InvertibleSet mempty (Set.singleton a)

union :: Ord a => InvertibleSet a -> InvertibleSet a -> InvertibleSet a
union = (<>)

difference :: Ord a => InvertibleSet a -> InvertibleSet a -> InvertibleSet a
difference a b = a <> invert b

fromList :: Ord a => [a] -> InvertibleSet a
fromList = mconcat . map singleton

toList :: Ord a => InvertibleSet a -> [a]
toList = Set.toList . unInvertibleSet
