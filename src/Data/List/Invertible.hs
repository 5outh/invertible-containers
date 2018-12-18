module Data.List.Invertible
  ( InvertibleList
  , unInvertibleList
  , union
  , difference
  , singleton
  , negativeSingleton
  , fromList
  , toList
  )
where

import           Data.Group
import qualified Data.List                 as List
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data InvertibleList a = InvertibleList
  { getPositive :: [a]
  , getNegative :: [a]
  } deriving (Ord, Generic, Foldable)

instance (Show a, Ord a) => Show (InvertibleList a) where
  show s = show (unInvertibleList s)

instance (Ord a, Arbitrary a) => Arbitrary (InvertibleList a) where
  arbitrary = InvertibleList <$> arbitrary <*> arbitrary

unInvertibleList :: Ord a => InvertibleList a -> [a]
unInvertibleList is = getPositive is List.\\ getNegative is

instance (Ord a, Eq a) => Eq (InvertibleList a) where
  a == b = unInvertibleList a == unInvertibleList b

instance Ord a => Semigroup (InvertibleList a) where
  a <> b = InvertibleList
    { getPositive = getPositive a <> getPositive b
    , getNegative = getNegative a <> getNegative b
    }

instance Ord a => Monoid (InvertibleList a) where
  mempty = InvertibleList mempty mempty

instance Ord a => Group (InvertibleList a) where
  invert (InvertibleList p n) = InvertibleList n p

singleton :: Ord a => a -> InvertibleList a
singleton a = InvertibleList (pure a) mempty

negativeSingleton :: Ord a => a -> InvertibleList a
negativeSingleton a = InvertibleList mempty (pure a)

union :: Ord a => InvertibleList a -> InvertibleList a -> InvertibleList a
union = (<>)

difference :: Ord a => InvertibleList a -> InvertibleList a -> InvertibleList a
difference a b = a <> invert b

fromList :: Ord a => [a] -> InvertibleList a
fromList = mconcat . map singleton

toList :: Ord a => InvertibleList a -> [a]
toList = unInvertibleList
