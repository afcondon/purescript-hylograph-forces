-- | Snapshot and restore: everything that changes as a simulation runs, as
-- | one immutable value.
-- |
-- | The forces keep no state between ticks (each rebuilds what it needs from
-- | the nodes), so what changes is alpha, the random source, and each node's
-- | position, velocity and pins. Restoring a snapshot and running N ticks
-- | gives exactly what N more ticks would have given when it was taken, as
-- | long as the forces and settings are the same; they are configuration,
-- | not state, and are not in the snapshot.
-- |
-- | The Rust kernel has the same pair (`snapshot` and `restore` in
-- | `force-kernel/src/lib.rs`), and `test/differential.mjs` checks both
-- | against each other across a restore.
module Hylograph.Forces.Pure.Snapshot
  ( Snapshot
  , take
  , restore
  ) where

import Prelude

import Data.Array (fromFoldable, zip)
import Data.List (List(..), reverse)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, Pinnable, writePin, writePosition, writeVelocity)
import Hylograph.Forces.Pure.Random (Seed)
import Hylograph.Forces.Pure.Random as Random

-- | One node's state.
type Kept = { x :: Number, y :: Number, vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number }

-- | The state of a simulation at one moment.
newtype Snapshot = Snapshot { alpha :: Number, seed :: Seed, nodes :: Array Kept }

-- | Record the simulation as it is: the alpha to give the next tick, the
-- | random source, and the nodes.
take :: forall row. Random.Source -> Number -> Array (Node (Pinnable row)) -> Effect Snapshot
take random alpha nodes = do
  seed <- Ref.read random
  kept <- Ref.new Nil
  foreachE nodes \node -> do
    earlier <- Ref.read kept
    Ref.write (Cons { x: node.x, y: node.y, vx: node.vx, vy: node.vy, fx: node.fx, fy: node.fy } earlier) kept
  newestFirst <- Ref.read kept
  pure (Snapshot { alpha, seed, nodes: fromFoldable (reverse newestFirst) })

-- | Put the nodes and the random source back as the snapshot recorded
-- | them, and return the alpha to give the next tick. The nodes are matched
-- | by place; if there are more or fewer than were recorded, the extra ones
-- | on either side are left alone.
restore :: forall row. Snapshot -> Random.Source -> Array (Node (Pinnable row)) -> Effect Number
restore (Snapshot snapshot) random nodes = do
  Ref.write snapshot.seed random
  foreachE (zip nodes snapshot.nodes) \(Tuple node kept) -> do
    runEffectFn3 writePosition node kept.x kept.y
    runEffectFn3 writeVelocity node kept.vx kept.vy
    runEffectFn3 writePin node kept.fx kept.fy
  pure snapshot.alpha
