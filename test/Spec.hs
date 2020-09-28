{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Data.Group
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Bag                   as Bag
import           Data.Functor.Compose
import qualified Data.List.Invertible       as List
import qualified Data.Map.Lazy.Invertible   as MapLazy
import qualified Data.Map.Strict.Invertible as MapStrict
import           Data.Monoid
import           Data.Signed                (Signed)
import qualified Data.Signed                as Signed

type Associative m = m -> m -> m -> Bool
type Identity m = m -> Bool
type Invertible m = m -> Bool

associative :: (Eq s, Semigroup s) => s -> s -> s -> Bool
associative a b c = (a <> b) <> c == a <> (b <> c)

leftIdentity :: (Eq m, Monoid m) => m -> Bool
leftIdentity a = a <> mempty == a

rightIdentity :: (Eq m, Monoid m) => m -> Bool
rightIdentity a = mempty <> a == a

invertible :: (Eq g, Group g) => g -> Bool
invertible a = a <> invert a == mempty

satisfiesGroup
  :: forall k. (Eq (k Int), Group (k Int), Arbitrary (k Int), Show (k Int))
  => Spec
satisfiesGroup = do
  it "is a semigroup"
    $ property (associative :: Associative (k Int))
  describe "is a monoid" $ do
    it "left identity" $
      property (leftIdentity :: Identity (k Int))
    it "right identity" $
      property (rightIdentity :: Identity (k Int))
  it "is a group"
    $ property (invertible :: Invertible (k Int))

main :: IO ()
main = hspec $ do
  describe "bag" $ satisfiesGroup @Bag.Bag
  describe "invertible map (strict)" $ satisfiesGroup @(MapStrict.InvertibleMap Int)
  describe "invertible map (lazy)" $ satisfiesGroup @(MapLazy.InvertibleMap Int)
  describe "invertible list" $ satisfiesGroup @List.InvertibleList
  -- describe "Signed" $ satisfiesGroup @(Compose Signed Sum)
    -- it "is a semigroup"
      -- $ property (associative :: Associative (Signed (Sum Int)))
    -- -- it "is a monoid" $ do
      -- -- property (leftIdentity :: Identity (Signed (Sum Int)))
      -- -- property (rightIdentity :: Identity (Signed (Sum Int)))
    -- it "is a group" $ property (invertible :: Invertible (Signed (Sum Int)))
