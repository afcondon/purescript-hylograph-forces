-- | The centering force: shift every node so that their mean position sits
-- | at a given point. It moves positions, not velocities.
-- |
-- | Ported from d3-force 3.0.0, `center.js` (ISC, Copyright 2010-2021 Mike
-- | Bostock); see LICENSE.
module Hylograph.Forces.Pure.Center
  ( Config
  , defaultConfig
  , Center
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (length)
import Data.Int (toNumber)
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, writePosition)

-- | d3's `forceCenter` settings.
type Config = { x :: Number, y :: Number, strength :: Number }

-- | d3's defaults: the origin, at full strength.
defaultConfig :: Config
defaultConfig = { x: 0.0, y: 0.0, strength: 1.0 }

-- | An initialized centering force.
newtype Center row = Center { nodes :: Array (Node row), config :: Config }

-- | d3's `force.initialize(nodes)`.
initialize :: forall row. Config -> Array (Node row) -> Center row
initialize config nodes = Center { nodes, config }

-- | d3's `force()`. The sums are taken over all the nodes before any node
-- | moves, and taken when the force is applied: the nodes are mutable, so
-- | reading them is part of the effect.
apply :: forall row. Center row -> Effect Unit
apply (Center force) = do
  sum <- Ref.new { x: 0.0, y: 0.0 }
  foreachE force.nodes \node -> do
    total <- Ref.read sum
    Ref.write { x: total.x + node.x, y: total.y + node.y } sum
  total <- Ref.read sum
  let
    count = toNumber (length force.nodes)
    shiftX = (total.x / count - force.config.x) * force.config.strength
    shiftY = (total.y / count - force.config.y) * force.config.strength
  foreachE force.nodes \node ->
    runEffectFn3 writePosition node (node.x - shiftX) (node.y - shiftY)
