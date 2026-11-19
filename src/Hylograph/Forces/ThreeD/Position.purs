-- | The positioning forces in three dimensions: pull each node towards a
-- | target x, y or z.
-- |
-- | Ported from d3-force-3d 3.0.6, `x.js`, `y.js` and `z.js` (MIT, Copyright
-- | 2017 Vasco Asturiano; itself from d3-force, ISC, Mike Bostock); see
-- | LICENSE. `x.js` and `y.js` are d3-force's unchanged; `z.js` is the same
-- | on the third axis.
module Hylograph.Forces.ThreeD.Position
  ( Config
  , defaultConfig
  , Position
  , initialize
  , applyX
  , applyY
  , applyZ
  ) where

import Prelude

import Data.Number (isNaN)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn4)
import Hylograph.Forces.ThreeD.Node (Node, writeVelocity)

-- | `forceX` / `forceY` / `forceZ` settings. `target` and `strength` are
-- | asked once per node when the force is initialized.
type Config row = { target :: Node row -> Number, strength :: Node row -> Number }

-- | d3's defaults: towards zero, at strength 0.1.
defaultConfig :: forall row. Config row
defaultConfig = { target: \_ -> 0.0, strength: \_ -> 0.1 }

-- | A node with the coordinate it is pulled towards.
type Pulled row = { node :: Node row, target :: Number, strength :: Number }

-- | An initialized positioning force. The axis it acts along is decided by
-- | which of `applyX`, `applyY` and `applyZ` it is given to.
newtype Position row = Position (Array (Pulled row))

-- | d3's `force.initialize(nodes)`. A node whose target is `NaN` gets
-- | strength zero.
initialize :: forall row. Config row -> Array (Node row) -> Position row
initialize config nodes = Position (map pulled nodes)
  where
  pulled node =
    let
      target = config.target node
    in
      { node, target, strength: if isNaN target then 0.0 else config.strength node }

-- | `node.vx += (target - node.x) * strength * alpha`.
applyX :: forall row. Position row -> Number -> Effect Unit
applyX (Position nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn4 writeVelocity pulled.node
      (pulled.node.vx + (pulled.target - pulled.node.x) * pulled.strength * alpha)
      pulled.node.vy
      pulled.node.vz

-- | `node.vy += (target - node.y) * strength * alpha`.
applyY :: forall row. Position row -> Number -> Effect Unit
applyY (Position nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn4 writeVelocity pulled.node
      pulled.node.vx
      (pulled.node.vy + (pulled.target - pulled.node.y) * pulled.strength * alpha)
      pulled.node.vz

-- | `node.vz += (target - node.z) * strength * alpha`.
applyZ :: forall row. Position row -> Number -> Effect Unit
applyZ (Position nodes) alpha =
  foreachE nodes \pulled ->
    runEffectFn4 writeVelocity pulled.node
      pulled.node.vx
      pulled.node.vy
      (pulled.node.vz + (pulled.target - pulled.node.z) * pulled.strength * alpha)
