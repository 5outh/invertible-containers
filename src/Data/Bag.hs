{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Bag
  ( Bag
  , unBag
  , union
  , difference
  , singleton
  , negativeSingleton
  , fromList
  , toList
  )
where

import           Data.Group
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Semigroup            (Sum (..))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data Bag a = Bag
  { getPositive :: Map a (Sum Int)
  , getNegative :: Map a (Sum Int)
  } deriving (Ord, Generic, Show)

mapDiff :: Ord a => Map a (Sum Int) -> Map a (Sum Int) -> Set a
mapDiff a b = Set.fromList $ catMaybes $ Map.elems $ flip Map.mapWithKey a $ \k ct ->
  case Map.lookup k b of
    Nothing -> Just k
    Just ct2 -> if
      (ct-ct2) <= 0 then Nothing
      else Just k

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary = Bag <$> arbitrary <*> arbitrary

unBag :: Ord a => Bag a -> Set a
unBag is = getPositive is `mapDiff` getNegative is

instance (Ord a, Eq a) => Eq (Bag a) where
  a == b = unBag a == unBag b

-- want to be able to "bring back" an element. how can that be done??? sum it
instance Ord a => Semigroup (Bag a) where
  a <> b = Bag
    { getPositive = Map.unionWith (<>) (getPositive a) (getPositive b)
    , getNegative = Map.unionWith (<>) (getNegative a) (getNegative b)
    }

fromList :: Ord a => [a] -> Bag a
fromList = foldMap singleton

instance Ord a => Monoid (Bag a) where
  mempty = Bag mempty mempty

instance Ord a => Group (Bag a) where
  invert (Bag p n) = Bag n p

singleton :: Ord a => a -> Bag a
singleton a = Bag (Map.singleton a 1) mempty

negativeSingleton :: Ord a => a -> Bag a
negativeSingleton a = Bag mempty (Map.singleton a 1)

union :: Ord a => Bag a -> Bag a -> Bag a
union = (<>)

difference :: Ord a => Bag a -> Bag a -> Bag a
difference a b = a <> invert b

toList :: Ord a => Bag a -> [a]
toList = Set.toList . unBag
