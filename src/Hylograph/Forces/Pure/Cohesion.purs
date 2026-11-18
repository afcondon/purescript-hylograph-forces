-- | Group cohesion: pull each node toward the centroid of its group.
-- |
-- | Not in d3. The rule is the Rust kernel's (`apply_cohesion` in
-- | `force-kernel/src/forces.rs`), and the two are held to each other bit
-- | for bit by `test/differential.mjs`.
-- |
-- | The centroid of a group is the mean position of its members whose
-- | position is finite, summed in node order. For every node in a group
-- | whose position is finite:
-- |
-- | ```
-- | v += (centroid - position) * strength * alpha
-- | ```
-- |
-- | A node in no group is left alone and counts toward no centroid. A
-- | strength of `NaN` counts as zero, and such a node still counts toward
-- | its group's centroid.
module Hylograph.Forces.Pure.Cohesion
  ( Config
  , Cohesion
  , initialize
  , apply
  ) where

import Prelude

import Data.Number (isFinite, isNaN)
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Group (Membership, grouped)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)

-- | `group` and `strength` are asked once per node when the force is
-- | initialized.
type Config row =
  { group :: Node row -> Membership
  , strength :: Node row -> Number
  }

-- | A node of a group.
type Member row = { node :: Node row, strength :: Number }

-- | An initialized cohesion force: the members of each group, in node order.
newtype Cohesion row = Cohesion (Array (Array (Member row)))

initialize :: forall row. Config row -> Array (Node row) -> Cohesion row
initialize config nodes = Cohesion (map (map member) (grouped config.group nodes))
  where
  member node =
    let
      strength = config.strength node
    in
      { node, strength: if isNaN strength then 0.0 else strength }

-- | The force only changes velocities, so each group's centroid can be
-- | taken just before its members are pulled.
apply :: forall row. Cohesion row -> Number -> Effect Unit
apply (Cohesion groups) alpha =
  foreachE groups \members -> do
    sum <- Ref.new { x: 0.0, y: 0.0, count: 0.0 }
    foreachE members \member ->
      when (isFinite member.node.x && isFinite member.node.y) do
        total <- Ref.read sum
        Ref.write { x: total.x + member.node.x, y: total.y + member.node.y, count: total.count + 1.0 } sum
    total <- Ref.read sum
    let
      centroidX = total.x / total.count
      centroidY = total.y / total.count
    foreachE members \member ->
      when (isFinite member.node.x && isFinite member.node.y) do
        runEffectFn3 writeVelocity member.node
          (member.node.vx + (centroidX - member.node.x) * member.strength * alpha)
          (member.node.vy + (centroidY - member.node.y) * member.strength * alpha)
