import           Data.Map.Strict.Invertible (InvertibleMap)
import qualified Data.Map.Strict.Invertible as InvertibleMap
import           Data.Set.Invertible        (InvertibleSet)
import qualified Data.Set.Invertible        as InvertibleSet

--For example:
-- Nothing `diff` (Just "foo") == Nothing
-- (Just "foo") `diff` Nothing == Just "foo"
--
--
-- negate (Just "foo") ~ Nothing
-- negate Nothing ~ ??
