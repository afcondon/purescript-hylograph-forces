-- | The centring force in three dimensions: shift every node so that their
-- | mean position sits at a given point. It moves positions, not velocities.
-- |
-- | Ported from d3-force-3d 3.0.6, `center.js` (MIT, Copyright 2017 Vasco
-- | Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE.
-- |
-- | Not d3-force's `center.js` with a third coordinate: d3-force-3d counts a
-- | coordinate that is `NaN` as zero in the sum (`sx += node.x || 0`), and
-- | leaves an axis alone when its shift is zero or `NaN` (`if (sx) node.x
-- | -= sx`), which also keeps a `-0` coordinate `-0`.
module Hylograph.Forces.ThreeD.Center
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
import Effect.Uncurried (runEffectFn4)
import Hylograph.Forces.ThreeD.Node (Node, writePosition)

-- | d3-force-3d's `forceCenter` settings.
type Config = { x :: Number, y :: Number, z :: Number, strength :: Number }

-- | d3-force-3d's defaults: the origin, at full strength.
defaultConfig :: Config
defaultConfig = { x: 0.0, y: 0.0, z: 0.0, strength: 1.0 }

-- | An initialized centring force.
newtype Center row = Center { nodes :: Array (Node row), config :: Config }

-- | d3's `force.initialize(nodes)`.
initialize :: forall row. Config -> Array (Node row) -> Center row
initialize config nodes = Center { nodes, config }

-- | d3-force-3d's `force()`:
-- |
-- | ```
-- | for (i = 0; i < n; ++i) node = nodes[i], sx += node.x || 0, sy += node.y || 0, sz += node.z || 0;
-- | for (sx = (sx / n - x) * strength, sy = ..., sz = ..., i = 0; i < n; ++i) {
-- |   node = nodes[i];
-- |   if (sx) { node.x -= sx }
-- |   if (sy) { node.y -= sy; }
-- |   if (sz) { node.z -= sz; }
-- | }
-- | ```
apply :: forall row. Center row -> Effect Unit
apply (Center force) = do
  sum <- Ref.new { x: 0.0, y: 0.0, z: 0.0 }
  foreachE force.nodes \node -> do
    total <- Ref.read sum
    Ref.write { x: total.x + orZero node.x, y: total.y + orZero node.y, z: total.z + orZero node.z } sum
  total <- Ref.read sum
  let
    count = toNumber (length force.nodes)
    shiftX = (total.x / count - force.config.x) * force.config.strength
    shiftY = (total.y / count - force.config.y) * force.config.strength
    shiftZ = (total.z / count - force.config.z) * force.config.strength
  foreachE force.nodes \node ->
    runEffectFn4 writePosition node (shifted node.x shiftX) (shifted node.y shiftY) (shifted node.z shiftZ)

-- | JavaScript's `value || 0`: the value, unless it is zero or `NaN`.
orZero :: Number -> Number
orZero value = if value < 0.0 || value > 0.0 then value else 0.0

-- | `if (shift) coordinate -= shift`.
shifted :: Number -> Number -> Number
shifted coordinate shift = if shift < 0.0 || shift > 0.0 then coordinate - shift else coordinate
