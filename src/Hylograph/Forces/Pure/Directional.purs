-- | The directional link constraint: keep each link's target at least a gap
-- | beyond its source along an axis (children below their parents).
-- |
-- | Not in d3. The rule is the Rust kernel's (`apply_directional` in
-- | `force-kernel/src/forces.rs`), and the two are held to each other bit
-- | for bit by `test/differential.mjs`.
-- |
-- | With `u` the unit axis, and working on anticipated positions
-- | `a = position + velocity`, for each iteration and each link in order,
-- | reading the velocities as the links before it left them:
-- |
-- | ```
-- | d = (a_target - a_source) . u
-- | if d < gap:
-- |   push = (gap - d) * strength * alpha
-- |   v_target += u * push * bias
-- |   v_source -= u * push * (1 - bias)
-- | ```
-- |
-- | where `bias` is d3's link bias, so the better connected end moves less.
-- | A link already satisfied is not touched: the force pushes apart and
-- | never pulls. A link whose gap or strength is `NaN` is left alone.
module Hylograph.Forces.Pure.Directional
  ( Config
  , Directional
  , initialize
  , initializeById
  , apply
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either)
import Data.Number (isFinite, isNaN, sqrt)
import Effect (Effect, forE, foreachE)
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Linkage (Endpoints, Identified, Joined, joinByPlace, joinById)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)

-- | `gap` and `strength` are asked once per link when the force is
-- | initialized. The axis need not be a unit vector: it is normalised, and
-- | an axis of zero or infinite length becomes (0, 1), which is "below" on a
-- | screen.
type Config link =
  { gap :: link -> Number
  , strength :: link -> Number
  , axis :: { x :: Number, y :: Number }
  , iterations :: Int
  }

-- | A link ready to apply.
type Ordered row =
  { source :: Node row
  , target :: Node row
  , gap :: Number
  , strength :: Number
  , bias :: Number
  }

-- | An initialized directional constraint.
newtype Directional row = Directional
  { links :: Array (Ordered row)
  , axisX :: Number
  , axisY :: Number
  , iterations :: Int
  }

-- | For links that name their nodes by place in the node array.
initialize :: forall row linkRow. Config (Endpoints linkRow) -> Array (Node row) -> Array (Endpoints linkRow) -> Directional row
initialize config nodes links = ordered config (joinByPlace nodes links)

-- | For links that name their nodes by id; a link to an id no node has is
-- | d3's error, `node not found: <id>`.
initializeById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Config (Identified id linkRow)
  -> Array (Node row)
  -> Array (Identified id linkRow)
  -> Either String (Directional row)
initializeById identify config nodes links = map (ordered config) (joinById identify nodes links)

ordered :: forall row link. Config link -> Array (Joined row link) -> Directional row
ordered config joined = Directional
  { links: filter usable (map link joined)
  , axisX: if proper then config.axis.x / axisLength else 0.0
  , axisY: if proper then config.axis.y / axisLength else 1.0
  , iterations: config.iterations
  }
  where
  axisLength = sqrt (config.axis.x * config.axis.x + config.axis.y * config.axis.y)
  proper = axisLength > 0.0 && isFinite axisLength

  link end =
    { source: end.source
    , target: end.target
    , gap: config.gap end.link
    , strength: config.strength end.link
    , bias: end.bias
    }

  usable candidate = not (isNaN candidate.gap || isNaN candidate.strength)

-- | The target is written before the source's velocity is read, so a link
-- | from a node to itself comes out as it does in the Rust.
apply :: forall row. Directional row -> Number -> Effect Unit
apply (Directional force) alpha =
  forE 0 force.iterations \_ ->
    foreachE force.links \link ->
      let
        x = link.target.x + link.target.vx - link.source.x - link.source.vx
        y = link.target.y + link.target.vy - link.source.y - link.source.vy
        along = x * force.axisX + y * force.axisY
      in
        when (along < link.gap) do
          let
            push = (link.gap - along) * link.strength * alpha
            share = 1.0 - link.bias
          runEffectFn3 writeVelocity link.target
            (link.target.vx + force.axisX * push * link.bias)
            (link.target.vy + force.axisY * push * link.bias)
          runEffectFn3 writeVelocity link.source
            (link.source.vx - force.axisX * push * share)
            (link.source.vy - force.axisY * push * share)
