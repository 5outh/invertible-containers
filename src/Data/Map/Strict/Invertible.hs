module Data.Map.Strict.Invertible where

import           Data.Group
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data InvertibleMap k a = InvertibleMap
  { getPositive :: Map k a
  , getNegative :: Map k a
  } deriving (Ord)

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
