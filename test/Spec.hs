import           Data.Group
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.List.Invertible       as List
import qualified Data.Map.Lazy.Invertible   as MapLazy
import qualified Data.Map.Strict.Invertible as MapStrict
import           Data.Monoid
import qualified Data.Set.Invertible        as Set

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

main :: IO ()
main = hspec $ do
  describe "invertible set" $ do
    it "is a semigroup"
      $ quickCheck (associative :: Associative (Set.InvertibleSet Int))
    it "is a monoid" $ do
      quickCheck (leftIdentity :: Identity (Set.InvertibleSet Int))
      quickCheck (rightIdentity :: Identity (Set.InvertibleSet Int))
    it "is a group"
      $ quickCheck (invertible :: Invertible (Set.InvertibleSet Int))

  describe "invertible map (strict)" $ do
    it "is a semigroup" $ quickCheck
      (associative :: Associative (MapStrict.InvertibleMap Int Int))
    it "is a monoid" $ do
      quickCheck (leftIdentity :: Identity (MapStrict.InvertibleMap Int Int))
      quickCheck (rightIdentity :: Identity (MapStrict.InvertibleMap Int Int))
    it "is a group" $ quickCheck
      (invertible :: Invertible (MapStrict.InvertibleMap Int Int))

  describe "invertible map (lazy)" $ do
    it "is a semigroup" $ quickCheck
      (associative :: Associative (MapLazy.InvertibleMap Int Int))
    it "is a monoid" $ do
      quickCheck (leftIdentity :: Identity (MapLazy.InvertibleMap Int Int))
      quickCheck (rightIdentity :: Identity (MapLazy.InvertibleMap Int Int))
    it "is a group"
      $ quickCheck (invertible :: Invertible (MapLazy.InvertibleMap Int Int))

  describe "invertible list" $ do
    it "is a semigroup"
      $ quickCheck (associative :: Associative (List.InvertibleList Int))
    it "is a monoid" $ do
      quickCheck (leftIdentity :: Identity (List.InvertibleList Int))
      quickCheck (rightIdentity :: Identity (List.InvertibleList Int))
    it "is a group"
      $ quickCheck (invertible :: Invertible (List.InvertibleList Int))
