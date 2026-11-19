-- | Measures of how settled a layout is. The definitions are the Rust
-- | kernel's (`kinetic_energy`, `max_displacement`, `max_acceleration` and
-- | `settled` in `force-kernel/src/lib.rs`), and `test/differential.mjs`
-- | holds the two to each other bit for bit.
-- |
-- | Kinetic energy is read off the nodes. The other three need to know
-- | where the nodes were, so they are kept by a `Watch`: tell it when a tick
-- | begins and when it ends, and ask it afterwards.
module Hylograph.Forces.Measure
  ( kineticEnergy
  , Watch
  , watch
  , forget
  , before
  , after
  , maxDisplacement
  , maxAcceleration
  , settled
  ) where

import Prelude

import Data.List (List(..), reverse)
import Data.Number (sqrt)
import Effect (Effect, foreachE)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Hylograph.Forces.Node (Node)

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

type Place = { x :: Number, y :: Number }

-- | What a watch remembers: where the nodes were when the last tick began;
-- | each node's step in the tick before the last; the largest step and the
-- | largest change of step in the last tick; and how many ticks (0, 1 or 2
-- | and more) it has seen since it was last told to forget.
type Watched =
  { began :: List Place
  , steps :: List Place
  , displacement :: Number
  , acceleration :: Number
  , history :: Int
  }

-- | A record of the last two ticks.
newtype Watch = Watch (Ref Watched)

nothingSeen :: Watched
nothingSeen = { began: Nil, steps: Nil, displacement: 0.0, acceleration: 0.0, history: 0 }

-- | A watch that has seen nothing.
watch :: Effect Watch
watch = map Watch (Ref.new nothingSeen)

-- | Forget everything: after the nodes are replaced, restored from a
-- | snapshot or moved by hand, when what was seen no longer applies.
forget :: Watch -> Effect Unit
forget (Watch seen) = Ref.write nothingSeen seen

-- | Call as a tick begins: note where the nodes are.
before :: forall row. Watch -> Array (Node row) -> Effect Unit
before (Watch seen) nodes = do
  places <- placesOf (\node -> { x: node.x, y: node.y }) nodes
  Ref.modify_ (_ { began = places }) seen

-- | Call when the tick has ended: work out how far each node moved and how
-- | that differs from its step the tick before.
-- |
-- | The largest change of step is only meaningful from the second tick on;
-- | until then `maxAcceleration` is zero and `settled` is false.
after :: forall row. Watch -> Array (Node row) -> Effect Unit
after (Watch seen) nodes = do
  was <- Ref.read seen
  let comparable = was.history >= 1
  began <- Ref.new was.began
  earlier <- Ref.new was.steps
  steps <- Ref.new Nil
  furthest <- Ref.new 0.0
  sharpest <- Ref.new 0.0
  foreachE nodes \node -> do
    places <- Ref.read began
    case places of
      Nil -> pure unit
      Cons place rest -> do
        Ref.write rest began
        let
          stepX = node.x - place.x
          stepY = node.y - place.y
          moved2 = stepX * stepX + stepY * stepY
        soFar <- Ref.read furthest
        when (moved2 > soFar) (Ref.write moved2 furthest)
        previous <- Ref.read earlier
        case previous of
          Cons step others -> do
            Ref.write others earlier
            when comparable do
              let
                changeX = stepX - step.x
                changeY = stepY - step.y
                change2 = changeX * changeX + changeY * changeY
              worst <- Ref.read sharpest
              when (change2 > worst) (Ref.write change2 sharpest)
          Nil -> pure unit
        noted <- Ref.read steps
        Ref.write (Cons { x: stepX, y: stepY } noted) steps
  newestFirst <- Ref.read steps
  moved2 <- Ref.read furthest
  change2 <- Ref.read sharpest
  Ref.write
    { began: was.began
    , steps: reverse newestFirst
    , displacement: sqrt moved2
    , acceleration: sqrt change2
    , history: if comparable then 2 else 1
    }
    seen

-- | The furthest any node moved in the last tick: the largest distance
-- | between where a node was when the tick began and where it ended,
-- | everything included (forces, centring, clamps, pins). Zero before the
-- | first tick.
maxDisplacement :: Watch -> Effect Number
maxDisplacement (Watch seen) = map _.displacement (Ref.read seen)

-- | The largest change in any node's step between the last two ticks: the
-- | acceleration it actually underwent. A node on a spring is momentarily
-- | still at the far end of every swing, where energy and displacement
-- | vanish and this is at its largest. Zero until two ticks have been seen.
maxAcceleration :: Watch -> Effect Number
maxAcceleration (Watch seen) = do
  was <- Ref.read seen
  pure (if was.history < 2 then 0.0 else was.acceleration)

-- | Is the layout at rest? True when, over the last two ticks, no node moved
-- | further than `step` in a tick and no node's step changed by more than
-- | `acceleration`. False until two ticks have been seen.
settled :: { step :: Number, acceleration :: Number } -> Watch -> Effect Boolean
settled tolerance (Watch seen) = do
  was <- Ref.read seen
  pure (was.history >= 2 && was.displacement < tolerance.step && was.acceleration < tolerance.acceleration)

-- | One place per node, in node order.
placesOf :: forall row. (Node row -> Place) -> Array (Node row) -> Effect (List Place)
placesOf placeOf nodes = do
  noted <- Ref.new Nil
  foreachE nodes \node -> do
    earlier <- Ref.read noted
    Ref.write (Cons (placeOf node) earlier) noted
  map reverse (Ref.read noted)
