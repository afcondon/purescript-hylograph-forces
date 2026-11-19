-- | The positioning forces: pull each node towards a target x or a target y.
-- |
-- | Ported from d3-force 3.0.0, `x.js` and `y.js` (ISC, Copyright 2010-2021
-- | Mike Bostock); see LICENSE.
module Hylograph.Forces.Position
  ( Config
  , defaultConfig
  , Position
  , initialize
  , applyX
  , applyY
  ) where

import Prelude

import Data.Number (isNaN)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Node (Node, writeVelocity)

-- | d3's `forceX` / `forceY` settings. `target` and `strength` are asked
-- | once per node when the force is initialized.
type Config row = { target :: Node row -> Number, strength :: Node row -> Number }

-- | d3's defaults: towards zero, at strength 0.1.
defaultConfig :: forall row. Config row
defaultConfig = { target: \_ -> 0.0, strength: \_ -> 0.1 }

-- | A node with the coordinate it is pulled towards.
type Pulled row = { node :: Node row, target :: Number, strength :: Number }

-- | An initialized positioning force. Whether it acts along x or y is
-- | decided by which of `applyX` and `applyY` it is given to.
newtype Position row = Position (Array (Pulled row))

-- | d3's `force.initialize(nodes)`. A node whose target is `NaN` gets
-- | strength zero, as in d3.
initialize :: forall row. Config row -> Array (Node row) -> Position row
initialize config nodes = Position (map pulled nodes)
  where
  pulled node =
    let
      target = config.target node
    in
      { node, target, strength: if isNaN target then 0.0 else config.strength node }

-- | d3's `forceX` applied: `node.vx += (target - node.x) * strength * alpha`.
applyX :: forall row. Position row -> Number -> Effect Unit
applyX (Position nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn3 writeVelocity pulled.node
      (pulled.node.vx + (pulled.target - pulled.node.x) * pulled.strength * alpha)
      pulled.node.vy

-- | d3's `forceY` applied: `node.vy += (target - node.y) * strength * alpha`.
applyY :: forall row. Position row -> Number -> Effect Unit
applyY (Position nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn3 writeVelocity pulled.node
      pulled.node.vx
      (pulled.node.vy + (pulled.target - pulled.node.y) * pulled.strength * alpha)
