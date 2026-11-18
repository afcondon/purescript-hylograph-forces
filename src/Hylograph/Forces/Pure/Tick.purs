-- | The parts of a tick that are not forces: cooling, and moving the nodes.
-- |
-- | Ported from d3-force 3.0.0, `simulation.js` (ISC, Copyright 2010-2021
-- | Mike Bostock); see LICENSE.
module Hylograph.Forces.Pure.Tick
  ( defaultAlphaDecay
  , defaultVelocityDecay
  , cool
  , decay
  , initializeNodes
  , integrate
  , pin
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Number (cos, isNaN, pi, pow, sin, sqrt)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn2, runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, Pinnable, writeIndex, writePosition, writeVelocity)

-- | d3's default cooling rate: alpha falls from 1 to 0.001 in 300 ticks.
defaultAlphaDecay :: Number
defaultAlphaDecay = 1.0 - pow 0.001 (1.0 / 300.0)

-- | What d3 multiplies a velocity by each tick. (d3's `velocityDecay(0.4)`
-- | is the friction; this is one minus it.)
defaultVelocityDecay :: Number
defaultVelocityDecay = 0.6

-- | d3: `alpha += (alphaTarget - alpha) * alphaDecay`. Called before the
-- | forces of a tick, with the tick's forces given the result.
cool :: { alphaTarget :: Number, alphaDecay :: Number } -> Number -> Number
cool settings alpha = alpha + (settings.alphaTarget - alpha) * settings.alphaDecay

-- | d3's position update, after the forces of a tick:
-- |
-- | ```
-- | if (node.fx == null) node.x += node.vx *= velocityDecay;
-- | else node.x = node.fx, node.vx = 0;
-- | ```
-- |
-- | and the same for y. The position is written before the velocity, and
-- | neither write changes what the other one reads.
integrate :: forall row. Number -> Array (Node (Pinnable row)) -> Effect Unit
integrate velocityDecay nodes =
  foreachE nodes \node -> case toMaybe node.fx, toMaybe node.fy of
    Nothing, Nothing -> do
      runEffectFn3 writePosition node (node.x + node.vx * velocityDecay) (node.y + node.vy * velocityDecay)
      runEffectFn3 writeVelocity node (node.vx * velocityDecay) (node.vy * velocityDecay)
    Just fx, Nothing -> do
      runEffectFn3 writePosition node fx (node.y + node.vy * velocityDecay)
      runEffectFn3 writeVelocity node 0.0 (node.vy * velocityDecay)
    Nothing, Just fy -> do
      runEffectFn3 writePosition node (node.x + node.vx * velocityDecay) fy
      runEffectFn3 writeVelocity node (node.vx * velocityDecay) 0.0
    Just fx, Just fy -> do
      runEffectFn3 writePosition node fx fy
      runEffectFn3 writeVelocity node 0.0 0.0

-- | Cooling as `Hylograph.ForceEngine.Core.decayAlpha` has it: d3's step,
-- | and then zero once alpha has fallen below `alphaMin`, which is how that
-- | loop knows to stop.
decay :: { alphaMin :: Number, alphaTarget :: Number, alphaDecay :: Number } -> Number -> Number
decay settings alpha =
  let
    cooled = alpha + (settings.alphaTarget - alpha) * settings.alphaDecay
  in
    if cooled < settings.alphaMin then 0.0 else cooled

-- | d3's `initializeNodes`: give every node its index; put a pinned node on
-- | its pin; give a node with no position a place on d3's phyllotaxis
-- | spiral; and stop a node with no velocity.
-- |
-- | ```
-- | node.index = i;
-- | if (node.fx != null) node.x = node.fx;
-- | if (node.fy != null) node.y = node.fy;
-- | if (isNaN(node.x) || isNaN(node.y)) {
-- |   var radius = initialRadius * Math.sqrt(0.5 + i), angle = i * initialAngle;
-- |   node.x = radius * Math.cos(angle);
-- |   node.y = radius * Math.sin(angle);
-- | }
-- | if (isNaN(node.vx) || isNaN(node.vy)) node.vx = node.vy = 0;
-- | ```
-- |
-- | A field that is missing reads as `NaN` here, as it does in d3.
initializeNodes :: forall row. Array (Node (Pinnable (index :: Int | row))) -> Effect Unit
initializeNodes nodes =
  foreachE (mapWithIndex (\index node -> { index, node }) nodes) \placed -> do
    runEffectFn2 writeIndex placed.node placed.index
    runEffectFn3 writePosition placed.node
      ( case toMaybe placed.node.fx of
          Nothing -> placed.node.x
          Just fx -> fx
      )
      ( case toMaybe placed.node.fy of
          Nothing -> placed.node.y
          Just fy -> fy
      )
    when (isNaN placed.node.x || isNaN placed.node.y) do
      let
        radius = 10.0 * sqrt (0.5 + toNumber placed.index)
        angle = toNumber placed.index * initialAngle
      runEffectFn3 writePosition placed.node (radius * cos angle) (radius * sin angle)
    when (isNaN placed.node.vx || isNaN placed.node.vy) do
      runEffectFn3 writeVelocity placed.node 0.0 0.0

-- | d3's `initialAngle`, the golden angle. A value of its own, as in d3, and
-- | not written out where it is used: the optimising backend treats
-- | multiplication as associative and turns `i * (pi * k)` into `i * pi * k`,
-- | which is a different number.
initialAngle :: Number
initialAngle = pi * (3.0 - sqrt 5.0)

-- | Put every pinned coordinate back on its pin and stop it there: what the
-- | Rust kernel does after a hard containment, so that a pin beats a region.
pin :: forall row. Array (Node (Pinnable row)) -> Effect Unit
pin nodes =
  foreachE nodes \node -> case toMaybe node.fx, toMaybe node.fy of
    Nothing, Nothing -> pure unit
    Just fx, Nothing -> do
      runEffectFn3 writePosition node fx node.y
      runEffectFn3 writeVelocity node 0.0 node.vy
    Nothing, Just fy -> do
      runEffectFn3 writePosition node node.x fy
      runEffectFn3 writeVelocity node node.vx 0.0
    Just fx, Just fy -> do
      runEffectFn3 writePosition node fx fy
      runEffectFn3 writeVelocity node 0.0 0.0
