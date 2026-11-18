-- | The towards force: a spring from each node to its own target point.
-- |
-- | Not in d3. The rule is the Rust kernel's (`apply_towards` in
-- | `force-kernel/src/forces.rs`), and the two are held to each other bit
-- | for bit by `test/differential.mjs`. For every node with a target whose
-- | coordinates are both numbers:
-- |
-- | ```
-- | vx += (targetX - x) * strength * alpha
-- | vy += (targetY - y) * strength * alpha
-- | ```
-- |
-- | A node whose target has a `NaN` in it is left alone, and a strength of
-- | `NaN` counts as zero.
module Hylograph.Forces.Pure.Towards
  ( Config
  , Towards
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (filter)
import Data.Number (isNaN)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)

-- | `target` and `strength` are asked once per node when the force is
-- | initialized. To move the targets, initialize again.
type Config row =
  { target :: Node row -> { x :: Number, y :: Number }
  , strength :: Node row -> Number
  }

-- | A node with the point it is pulled towards.
type Pulled row = { node :: Node row, x :: Number, y :: Number, strength :: Number }

-- | An initialized towards force.
newtype Towards row = Towards (Array (Pulled row))

initialize :: forall row. Config row -> Array (Node row) -> Towards row
initialize config nodes = Towards (filter aimed (map pulled nodes))
  where
  pulled node =
    let
      target = config.target node
      strength = config.strength node
    in
      { node, x: target.x, y: target.y, strength: if isNaN strength then 0.0 else strength }

  aimed pulled = not (isNaN pulled.x || isNaN pulled.y)

apply :: forall row. Towards row -> Number -> Effect Unit
apply (Towards nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn3 writeVelocity pulled.node
      (pulled.node.vx + (pulled.x - pulled.node.x) * pulled.strength * alpha)
      (pulled.node.vy + (pulled.y - pulled.node.y) * pulled.strength * alpha)
