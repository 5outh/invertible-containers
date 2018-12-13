# invertible-containers

Containers that can be inverted. For example:

```haskell
λ> import qualified Data.Set.Invertible as ISet
λ> ISet.fromList [1,2,3] <> invert (ISet.fromList [1,2])
fromList [3]
```

## Motivation

Much like the ubiquitous `Monoid`, `Group`s happen to be pretty nifty. Monoids
give us nice semantics for append-only state-changes, but what if we want to
do something destructive? `Group`s let us _invert_ things, which turns out to be
quite useful for some containers. Consider the following scenario:

```haskell
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set

-- Players "draw straws" from a set. Player with the biggest one wins.
data DrawStraws = DrawStraws
  { remainingStraws :: Set Int
  , players         :: Map Int Player
  } deriving (Show)

instance Semigroup DrawStraws where
  ds1 <> ds2 = DrawStraws
    { remainingStraws = remainingStraws ds1 <> remainingStraws ds2
    , players = Map.unionWith (<>) (players ds1) (players ds2)
    }

instance Monoid DrawStraws where
  mempty = DrawStraws mempty mempty

newtype Player = Player
  { straws :: Set Int
  } deriving (Show)

instance Semigroup Player where
  p1 <> p2 = Player { straws = straws p1 <> straws p2 }

instance Monoid Player where
  mempty = Player mempty

-- 'chosenStraw' would probably be chosen randomly in Real Life
takeStraw :: Int -> Int -> DrawStraws -> DrawStraws
takeStraw playerId chosenStraw drawStraws = newDrawStraws <> updateDrawStraws
 where
  playerMod        = mempty { straws = Set.singleton chosenStraw }
  updateDrawStraws = mempty { players = Map.singleton playerId playerMod }

  -- Note the destructive update on 'drawStraws'
  newDrawStraws    = drawStraws
    { remainingStraws = Set.delete chosenStraw (remainingStraws drawStraws)
    }
```

```haskell
λ> let drawStraws = DrawStraws (Set.fromList [1,2,3,4,5]) (Map.fromList [(1, mempty), (2, mempty)])
λ> takeStraw 1 3 drawStraws
DrawStraws
  { remainingStraws = fromList [1,2,4,5]
  , players = fromList
      [ (1,Player {straws = fromList [3]})
      , (2,Player {straws = fromList []})
      ]
  }
```

We're _pretty close_ to a beautiful solution here, in that we can represent `Player`
updates as diffs using its `Semigroup` instance. However, that benefit gets
pretty much destroyed by having to destructively modify `remainingStraws`.

This is a contrived example, but it makes for a difficult situation in practice:
some definitions of state changes can use `mempty` to build up a minimal diff,
while others have to manually modify the existing state. In an ideal world,
we wouldn't have to make that choice: every state change could be represented
by a minimal state diff. That's where `invertible-containers` comes in.

```haskell
import Data.Set.Invertible as ISet

data DrawStraws = DrawStraws
  { remainingStraws :: ISet.InvertibleSet Int -- Use an Invertible Set
  , players :: Map Int Player
  }

instance Semigroup DrawStraws -- ...
instance Monoid DrawStraws -- ...

newtype Player = Player
  { straws :: Set Int
  }

instance Semigroup Player -- ...
instance Monoid Player -- ...


-- Now we can just mappend our state change!
takeStraw :: Int -> Int -> DrawStraws -> DrawStraws
takeStraw playerId chosenStraw = mappend updateDrawStraws
 where
  playerMod = mempty { straws = Set.singleton chosenStraw }
  updateDrawStraws =  mempty
    { players = Map.singleton playerId playerMod
    -- 'negativeSingleton' inserts a "removed" element into a set
    , remainingStraws = ISet.negativeSingleton chosenStraw
    }
```

```haskell
λ> import Data.Set.Invertible as ISet
λ> let drawStraws = DrawStraws (ISet.fromList [1,2,3,4,5]) (Map.fromList [(1, mempty), (2, mempty)])
λ> takeStraw 1 3 drawStraws
DrawStraws
  { remainingStraws = fromList [1,2,4,5]
  , players = fromList
      [ (1,Player {straws = fromList [3]})
      , (2,Player {straws = fromList []})
      ]
  }
```

Now we can just `mappend` the state update, and get what we want. That is the
purpose of this library. Assuming players could disappear, one might consider
making the `players` `Map` invertible as well! Similarly if straws could
magically disappear from players' hands. One might also consider using
invertible containers to help instantiate `Group` for larger state entities,
which gets you state diffing for free.
