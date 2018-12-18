{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Data.Signed where

import           Data.Group
import           Test.QuickCheck.Arbitrary

data Signed a = Signed
  { getNegative :: a
  , getPositive :: a
  }
  deriving (Show)

instance (Eq a, Group a) => Eq (Signed a) where
  s1 == s2 = unsigned s1 == unsigned s2

instance Arbitrary a => Arbitrary (Signed a) where
  arbitrary = Signed <$> arbitrary <*> arbitrary

instance Semigroup a => Semigroup (Signed a) where
  a <> b = Signed
    { getPositive = getPositive a <> getPositive b
    , getNegative = getNegative a <> getNegative b
    }

instance Monoid a => Monoid (Signed a) where
  mempty = Signed mempty mempty

-- We only need a '@Monoid@' for '@Signed@' to be a '@Group@'. In order to
-- check equivalence (and verify laws), we must wrap a '@Group@'. However,
-- the contraint can be relaxed for this instance, which is helpful.
-- For example:
--
-- >>> > positive Nothing <> invert (positive (Just "foo"))
-- >>> Signed {getNegative = Just "foo", getPositive = Nothing}
-- >>> > positive Nothing <> negative (Just "foo") <> positive (Just "blah")
-- >>> Signed {getNegative = Just "foo", getPositive = Just "blah"}
--
-- This allows us to access a diff between two values without necessarily
-- needing to diff the _internal_ value, which is very helpful.
--
instance Monoid a => Group (Signed a) where
  invert (Signed n p) = Signed p n

positive :: Monoid a => a -> Signed a
positive a = Signed mempty a

negative :: Monoid a => a -> Signed a
negative a = Signed a mempty

unsigned :: Group a => Signed a -> a
unsigned (Signed n p) = p <> invert n
