-- | The simulation node in three dimensions: a JavaScript object typed as a
-- | record, which the forces update in place, as `Hylograph.Forces.Node` is
-- | in two.
-- |
-- | The foreign code is the same four assignments as in two dimensions, with
-- | a third coordinate (`Node.js`). The same rule holds: a mutable field is
-- | read where its value is used, in the arguments of the write that replaces
-- | it or inside a pure function that returns before anything is written.
module Hylograph.Forces.ThreeD.Node
  ( Node
  , Pinnable
  , writeVelocity
  , writePosition
  , writePin
  , writeIndex
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn2, EffectFn4)

-- | A node of the simulation: position and velocity in three dimensions, and
-- | whatever else the application keeps on it.
type Node row =
  { x :: Number
  , y :: Number
  , z :: Number
  , vx :: Number
  , vy :: Number
  , vz :: Number
  | row
  }

-- | The fields that pin a node: where `fx`, `fy` or `fz` is present the
-- | integration step puts the node there on that axis and stops it. Absent,
-- | `null` and `undefined` all mean unpinned, as in d3-force-3d.
type Pinnable row = (fx :: Nullable Number, fy :: Nullable Number, fz :: Nullable Number | row)

-- | `node.vx = vx; node.vy = vy; node.vz = vz`
foreign import writeVelocity :: forall row. EffectFn4 (Node row) Number Number Number Unit

-- | `node.x = x; node.y = y; node.z = z`
foreign import writePosition :: forall row. EffectFn4 (Node row) Number Number Number Unit

-- | `node.fx = fx; node.fy = fy; node.fz = fz`
foreign import writePin :: forall row. EffectFn4 (Node (Pinnable row)) (Nullable Number) (Nullable Number) (Nullable Number) Unit

-- | `node.index = index`, for d3-force-3d's `initializeNodes`. As in two
-- | dimensions, the row is not asked to contain `index`.
foreign import writeIndex :: forall row. EffectFn2 (Node row) Int Unit
