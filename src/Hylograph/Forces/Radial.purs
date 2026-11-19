-- | The radial force: pull each node towards a circle.
-- |
-- | Ported from d3-force 3.0.0, `radial.js` (ISC, Copyright 2010-2021 Mike
-- | Bostock); see LICENSE.
module Hylograph.Forces.Radial
  ( Config
  , defaultConfig
  , Radial
  , initialize
  , apply
  ) where

import Prelude

import Data.Number (isNaN, sqrt)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Node (Node, writeVelocity)

-- | d3's `forceRadial` settings. `radius` and `strength` are asked once per
-- | node when the force is initialized.
type Config row =
  { radius :: Node row -> Number
  , strength :: Node row -> Number
  , x :: Number
  , y :: Number
  }

-- | d3's defaults for everything but the radius, which d3 requires: here a
-- | circle of radius 100 about the origin, at strength 0.1.
defaultConfig :: forall row. Config row
defaultConfig = { radius: \_ -> 100.0, strength: \_ -> 0.1, x: 0.0, y: 0.0 }

-- | A node with the radius it is pulled towards.
type Pulled row = { node :: Node row, radius :: Number, strength :: Number }

-- | An initialized radial force.
newtype Radial row = Radial { nodes :: Array (Pulled row), x :: Number, y :: Number }

-- | d3's `force.initialize(nodes)`. A node whose radius is `NaN` gets
-- | strength zero, as in d3.
initialize :: forall row. Config row -> Array (Node row) -> Radial row
initialize config nodes = Radial { nodes: map pulled nodes, x: config.x, y: config.y }
  where
  pulled node =
    let
      radius = config.radius node
    in
      { node, radius, strength: if isNaN radius then 0.0 else config.strength node }

-- | d3's `force(alpha)`.
apply :: forall row. Radial row -> Number -> Effect Unit
apply (Radial force) alpha =
  foreachE force.nodes \pulled ->
    let
      -- d3: `dx = node.x - x || 1e-6`
      offsetX = orTiny (pulled.node.x - force.x)
      offsetY = orTiny (pulled.node.y - force.y)
      distance = sqrt (offsetX * offsetX + offsetY * offsetY)
      scale = (pulled.radius - distance) * pulled.strength * alpha / distance
    in
      runEffectFn3 writeVelocity pulled.node (pulled.node.vx + offsetX * scale) (pulled.node.vy + offsetY * scale)

-- | d3's `value || 1e-6`: the value, unless it is zero or `NaN`.
orTiny :: Number -> Number
orTiny value = if value < 0.0 || value > 0.0 then value else 1.0e-6
