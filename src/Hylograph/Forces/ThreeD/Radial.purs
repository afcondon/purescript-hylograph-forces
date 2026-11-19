-- | The radial force in three dimensions: pull each node towards a sphere.
-- |
-- | Ported from d3-force-3d 3.0.6, `radial.js` (MIT, Copyright 2017 Vasco
-- | Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE.
-- |
-- | Besides the third coordinate, d3-force-3d reads y and z as zero where
-- | they are `NaN` (`(node.y || 0) - y`); x is read as it is, as in d3-force.
module Hylograph.Forces.ThreeD.Radial
  ( Config
  , defaultConfig
  , Radial
  , initialize
  , apply
  ) where

import Prelude

import Data.Number (isNaN, sqrt)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn4)
import Hylograph.Forces.ThreeD.Node (Node, writeVelocity)

-- | `forceRadial` settings. `radius` and `strength` are asked once per node
-- | when the force is initialized.
type Config row =
  { radius :: Node row -> Number
  , strength :: Node row -> Number
  , x :: Number
  , y :: Number
  , z :: Number
  }

-- | d3's defaults for everything but the radius, which d3 requires: here a
-- | sphere of radius 100 about the origin, at strength 0.1.
defaultConfig :: forall row. Config row
defaultConfig = { radius: \_ -> 100.0, strength: \_ -> 0.1, x: 0.0, y: 0.0, z: 0.0 }

-- | A node with the radius it is pulled towards.
type Pulled row = { node :: Node row, radius :: Number, strength :: Number }

-- | An initialized radial force.
newtype Radial row = Radial { nodes :: Array (Pulled row), x :: Number, y :: Number, z :: Number }

-- | `force.initialize(nodes)`. A node whose radius is `NaN` gets strength
-- | zero.
initialize :: forall row. Config row -> Array (Node row) -> Radial row
initialize config nodes = Radial { nodes: map pulled nodes, x: config.x, y: config.y, z: config.z }
  where
  pulled node =
    let
      radius = config.radius node
    in
      { node, radius, strength: if isNaN radius then 0.0 else config.strength node }

-- | d3-force-3d's `force(alpha)`:
-- |
-- | ```
-- | dx = node.x - x || 1e-6,
-- | dy = (node.y || 0) - y || 1e-6,
-- | dz = (node.z || 0) - z || 1e-6,
-- | r = Math.sqrt(dx * dx + dy * dy + dz * dz),
-- | k = (radiuses[i] - r) * strengths[i] * alpha / r;
-- | node.vx += dx * k; node.vy += dy * k; node.vz += dz * k;
-- | ```
apply :: forall row. Radial row -> Number -> Effect Unit
apply (Radial force) alpha =
  foreachE force.nodes \pulled ->
    let
      offsetX = orTiny (pulled.node.x - force.x)
      offsetY = orTiny (orZero pulled.node.y - force.y)
      offsetZ = orTiny (orZero pulled.node.z - force.z)
      distance = sqrt (offsetX * offsetX + offsetY * offsetY + offsetZ * offsetZ)
      scale = (pulled.radius - distance) * pulled.strength * alpha / distance
    in
      runEffectFn4 writeVelocity pulled.node
        (pulled.node.vx + offsetX * scale)
        (pulled.node.vy + offsetY * scale)
        (pulled.node.vz + offsetZ * scale)

-- | `value || 1e-6`: the value, unless it is zero or `NaN`.
orTiny :: Number -> Number
orTiny value = if value < 0.0 || value > 0.0 then value else 1.0e-6

-- | `value || 0`.
orZero :: Number -> Number
orZero value = if value < 0.0 || value > 0.0 then value else 0.0
