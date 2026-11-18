-- | Measures of how settled a layout is. The definitions are the Rust
-- | kernel's (`kinetic_energy` and `max_displacement` in
-- | `force-kernel/src/lib.rs`), and `test/differential.mjs` holds the two
-- | to each other bit for bit.
module Hylograph.Forces.Pure.Measure
  ( kineticEnergy
  , Positions
  , remember
  , maxDisplacement
  ) where

import Prelude

import Data.Array (fromFoldable, zip)
import Data.List (List(..), reverse)
import Data.Number (sqrt)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Hylograph.Forces.Pure.Node (Node)

-- | Kinetic energy, taking every node to have unit mass: the sum over the
-- | nodes, in order, of `vx * vx` and then `vy * vy`, halved. After a tick
-- | the velocity is the step the node took in it, so this falls toward zero
-- | as the layout settles.
kineticEnergy :: forall row. Array (Node row) -> Effect Number
kineticEnergy nodes = do
  sum <- Ref.new 0.0
  foreachE nodes \node -> do
    total <- Ref.read sum
    Ref.write (total + node.vx * node.vx + node.vy * node.vy) sum
  total <- Ref.read sum
  pure (total / 2.0)

-- | Where the nodes were at some moment.
newtype Positions = Positions (Array { x :: Number, y :: Number })

-- | Note where the nodes are now, to measure from later.
remember :: forall row. Array (Node row) -> Effect Positions
remember nodes = do
  noted <- Ref.new Nil
  foreachE nodes \node -> do
    earlier <- Ref.read noted
    Ref.write (Cons { x: node.x, y: node.y } earlier) noted
  newestFirst <- Ref.read noted
  pure (Positions (fromFoldable (reverse newestFirst)))

-- | The furthest any node is from where it was remembered: with positions
-- | remembered before a tick, the furthest any node moved in it, everything
-- | included (forces, centring, clamps, pins).
maxDisplacement :: forall row. Positions -> Array (Node row) -> Effect Number
maxDisplacement (Positions before) nodes = do
  worst <- Ref.new 0.0
  foreachE (zip before nodes) \(Tuple was node) -> do
    furthest <- Ref.read worst
    let
      movedX = node.x - was.x
      movedY = node.y - was.y
      moved2 = movedX * movedX + movedY * movedY
    when (moved2 > furthest) (Ref.write moved2 worst)
  furthest <- Ref.read worst
  pure (sqrt furthest)
