-- | Which group a node is in, for the forces that act by group: cohesion
-- | and scoped many-body.
module Hylograph.Forces.Group
  ( Membership(..)
  , ungrouped
  , inGroup
  , grouped
  ) where

import Data.Array (fromFoldable, snoc)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)

-- | A node is in one group, named by a number, or in none.
data Membership
  = Ungrouped
  | InGroup Int

-- | In no group.
ungrouped :: Membership
ungrouped = Ungrouped

-- | In the group with this number.
inGroup :: Int -> Membership
inGroup = InGroup

-- | The members of each group: groups in ascending order of their number,
-- | members in the order they were given. Items in no group are left out.
grouped :: forall item. (item -> Membership) -> Array item -> Array (Array item)
grouped membership items = fromFoldable (Map.values (foldl place Map.empty items))
  where
  place groups item = case membership item of
    Ungrouped -> groups
    InGroup group -> Map.alter (add item) group groups

  add item members = case members of
    Nothing -> Just [ item ]
    Just earlier -> Just (snoc earlier item)
