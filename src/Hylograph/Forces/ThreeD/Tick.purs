-- | The parts of a tick that are not forces, in three dimensions: moving the
-- | nodes, and d3-force-3d's `initializeNodes`. Cooling has no dimension and
-- | is `Hylograph.Forces.Tick.cool`.
-- |
-- | Ported from d3-force-3d 3.0.6, `simulation.js` (MIT, Copyright 2017
-- | Vasco Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE.
module Hylograph.Forces.ThreeD.Tick
  ( integrate
  , initializeNodes
  , initialAngleRoll
  , initialAngleYaw
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Number (cos, isNaN, pi, sin, sqrt)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn2, runEffectFn4)
import Hylograph.Forces.ThreeD.Node (Node, Pinnable, writeIndex, writePosition, writeVelocity)

-- | `Math.cbrt`, which the `numbers` package does not have: a primitive of
-- | the language, as `sqrt`, `sin` and `cos` are in `Data.Number`, and the
-- | one d3-force-3d's initial placement calls. `pow x (1/3)` is a different
-- | number.
foreign import cbrt :: Number -> Number

-- | d3-force-3d's position update, after the forces of a tick, each axis on
-- | its own:
-- |
-- | ```
-- | if (node.fx == null) node.x += node.vx *= velocityDecay;
-- | else node.x = node.fx, node.vx = 0;
-- | ```
-- |
-- | and the same for y and z.
integrate :: forall row. Number -> Array (Node (Pinnable row)) -> Effect Unit
integrate velocityRetention nodes =
  foreachE nodes \node -> case toMaybe node.fx, toMaybe node.fy, toMaybe node.fz of
    Nothing, Nothing, Nothing -> do
      runEffectFn4 writePosition node
        (node.x + node.vx * velocityRetention)
        (node.y + node.vy * velocityRetention)
        (node.z + node.vz * velocityRetention)
      runEffectFn4 writeVelocity node
        (node.vx * velocityRetention)
        (node.vy * velocityRetention)
        (node.vz * velocityRetention)
    _, _, _ -> do
      runEffectFn4 writePosition node
        (moved velocityRetention node.fx node.x node.vx)
        (moved velocityRetention node.fy node.y node.vy)
        (moved velocityRetention node.fz node.z node.vz)
      runEffectFn4 writeVelocity node
        (kept velocityRetention node.fx node.vx)
        (kept velocityRetention node.fy node.vy)
        (kept velocityRetention node.fz node.vz)

-- | Where one coordinate goes: onto its pin, or along its velocity.
moved :: Number -> Nullable Number -> Number -> Number -> Number
moved velocityRetention pin position velocity = case toMaybe pin of
  Nothing -> position + velocity * velocityRetention
  Just fixed -> fixed

-- | What one component of the velocity becomes: zero on a pinned axis.
kept :: Number -> Nullable Number -> Number -> Number
kept velocityRetention pin velocity = case toMaybe pin of
  Nothing -> velocity * velocityRetention
  Just _ -> 0.0

-- | d3-force-3d's `initializeNodes` at three dimensions: give every node its
-- | index; put a pinned coordinate on its pin; give a node with no position
-- | a place on d3-force-3d's spherical spiral; and stop a node with no
-- | velocity.
-- |
-- | ```
-- | node.index = i;
-- | if (node.fx != null) node.x = node.fx;
-- | if (node.fy != null) node.y = node.fy;
-- | if (node.fz != null) node.z = node.fz;
-- | if (isNaN(node.x) || isNaN(node.y) || isNaN(node.z)) {
-- |   var radius = initialRadius * Math.cbrt(0.5 + i),
-- |       rollAngle = i * initialAngleRoll,
-- |       yawAngle = i * initialAngleYaw;
-- |   node.x = radius * Math.sin(rollAngle) * Math.cos(yawAngle);
-- |   node.y = radius * Math.cos(rollAngle);
-- |   node.z = radius * Math.sin(rollAngle) * Math.sin(yawAngle);
-- | }
-- | if (isNaN(node.vx) || isNaN(node.vy) || isNaN(node.vz)) node.vx = node.vy = node.vz = 0;
-- | ```
-- |
-- | A node with no position is placed whole: a node pinned in x alone, and
-- | nowhere yet, loses its pin's x to the spiral (and gets it back at the
-- | first tick), as in d3-force-3d. A field that is missing reads as `NaN`.
initializeNodes :: forall row. Array (Node (Pinnable row)) -> Effect Unit
initializeNodes nodes =
  foreachE (mapWithIndex (\index node -> { index, node }) nodes) \placed -> do
    runEffectFn2 writeIndex placed.node placed.index
    runEffectFn4 writePosition placed.node
      (pinnedOr placed.node.fx placed.node.x)
      (pinnedOr placed.node.fy placed.node.y)
      (pinnedOr placed.node.fz placed.node.z)
    when (isNaN placed.node.x || isNaN placed.node.y || isNaN placed.node.z) do
      let
        radius = 10.0 * cbrt (0.5 + toNumber placed.index)
        roll = toNumber placed.index * initialAngleRoll
        yaw = toNumber placed.index * initialAngleYaw
      runEffectFn4 writePosition placed.node
        (radius * sin roll * cos yaw)
        (radius * cos roll)
        (radius * sin roll * sin yaw)
    when (isNaN placed.node.vx || isNaN placed.node.vy || isNaN placed.node.vz) do
      runEffectFn4 writeVelocity placed.node 0.0 0.0 0.0
  where
  pinnedOr pin position = case toMaybe pin of
    Nothing -> position
    Just fixed -> fixed

-- | d3-force-3d's `initialAngleRoll`, the golden angle: a value of its own,
-- | for the reason given at `Hylograph.Forces.Tick.initializeNodes` (the
-- | optimising backend would regroup `i * (pi * k)`).
initialAngleRoll :: Number
initialAngleRoll = pi * (3.0 - sqrt 5.0)

-- | d3-force-3d's `initialAngleYaw`, `Math.PI * 20 / (9 + Math.sqrt(221))`.
initialAngleYaw :: Number
initialAngleYaw = pi * 20.0 / (9.0 + sqrt 221.0)
