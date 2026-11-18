-- | The simulation node, as the rest of Hylograph hands it over: a JavaScript
-- | object typed as a record, which the forces update in place.
-- |
-- | PureScript can read a record field but has no way to assign one, and
-- | nothing in the package set does it either (`Record.ST` left `record` at
-- | 4.0). So this module holds the package's only foreign code: four
-- | assignments, one line each, in `Node.js`. Every piece of arithmetic is
-- | PureScript.
-- |
-- | The fields are mutable behind the type system's back, so the forces follow
-- | one rule: a mutable field is read where its value is used, in the
-- | arguments of the write that replaces it or inside a pure function that
-- | returns before anything is written. It is never bound to a name across a
-- | write.
module Hylograph.Forces.Pure.Node
  ( Node
  , Pinnable
  , writeVelocity
  , writePosition
  , writePin
  , writeIndex
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn2, EffectFn3)

-- | A node of the simulation: position and velocity, and whatever else the
-- | application keeps on it.
type Node row = { x :: Number, y :: Number, vx :: Number, vy :: Number | row }

-- | The fields that pin a node: where `fx` or `fy` is present the integration
-- | step puts the node there and stops it. Absent, `null` and `undefined` all
-- | mean unpinned, as in d3.
type Pinnable row = (fx :: Nullable Number, fy :: Nullable Number | row)

-- | `node.vx = vx; node.vy = vy`
foreign import writeVelocity :: forall row. EffectFn3 (Node row) Number Number Unit

-- | `node.x = x; node.y = y`
foreign import writePosition :: forall row. EffectFn3 (Node row) Number Number Unit

-- | `node.fx = fx; node.fy = fy`, for restoring a snapshot.
foreign import writePin :: forall row. EffectFn3 (Node (Pinnable row)) (Nullable Number) (Nullable Number) Unit

-- | `node.index = index`, for d3's `initializeNodes`. d3 puts the index on
-- | every node whether or not the application's node type has a field for
-- | it, and so does this: the row is not asked to contain `index`.
foreign import writeIndex :: forall row. EffectFn2 (Node row) Int Unit
