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
-- |
-- | That is a force, scaled by alpha and shared with everything else that
-- | pushes, so the gap it leaves can be well short of the one asked for.
-- |
-- | HARD. With `hard: true` the gap asked for is the gap kept: after the
-- | nodes have moved, with the hard containments and in the order the
-- | forces are listed, the links are projected. A node is HELD along the
-- | axis if it is pinned on a coordinate the axis has a part in (`fx` when
-- | `u.x` is not 0, `fy` when `u.y` is not 0; here, whatever the config's
-- | `heldX` and `heldY` say). One pass takes each link in order, on the
-- | positions as the links before it left them, both ends finite:
-- |
-- | ```
-- | d = (p_target - p_source) . u
-- | if d < gap:
-- |   short = gap - d
-- |   if the target is not held:      p_target += u * short,  v_target += u * short
-- |   else if the source is not held: p_source -= u * short,  v_source -= u * short
-- |   else:                           nothing
-- | ```
-- |
-- | So the target gives way, and the source only where the target cannot.
-- | Passes repeat until one moves nothing, thirty-two at most (a chain of
-- | links listed child-first needs one pass per link; a cycle of links can
-- | never be satisfied and stops at the limit). The velocity changes with
-- | the position, as a containment's clamp does, so the node does not fly
-- | back next tick. The soft force above is applied as well, unless its
-- | strengths are 0.
module Hylograph.Forces.Directional
  ( Config
  , Directional
  , initialize
  , initializeById
  , apply
  , isHard
  , constrain
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either)
import Data.Number (isFinite, isNaN, sqrt)
import Effect (Effect, forE, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Linkage (Endpoints, Identified, Joined, joinByPlace, joinById)
import Hylograph.Forces.Node (Node, writePosition, writeVelocity)

-- | `gap` and `strength` are asked once per link when the force is
-- | initialized. The axis need not be a unit vector: it is normalised, and
-- | an axis of zero or infinite length becomes (0, 1), which is "below" on a
-- | screen. `heldX` and `heldY` matter only to the hard projection, and are
-- | asked each time it runs: `Node.pinnedX` and `Node.pinnedY` are the ones
-- | to give for nodes with `fx` and `fy`.
type Config row link =
  { gap :: link -> Number
  , strength :: link -> Number
  , axis :: { x :: Number, y :: Number }
  , iterations :: Int
  , hard :: Boolean
  , heldX :: Node row -> Boolean
  , heldY :: Node row -> Boolean
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
  , hard :: Boolean
  , heldX :: Node row -> Boolean
  , heldY :: Node row -> Boolean
  }

-- | For links that name their nodes by place in the node array.
initialize :: forall row linkRow. Config row (Endpoints linkRow) -> Array (Node row) -> Array (Endpoints linkRow) -> Directional row
initialize config nodes links = ordered config (joinByPlace nodes links)

-- | For links that name their nodes by id; a link to an id no node has is
-- | d3's error, `node not found: <id>`.
initializeById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Config row (Identified id linkRow)
  -> Array (Node row)
  -> Array (Identified id linkRow)
  -> Either String (Directional row)
initializeById identify config nodes links = map (ordered config) (joinById identify nodes links)

ordered :: forall row link. Config row link -> Array (Joined row link) -> Directional row
ordered config joined = Directional
  { links: filter usable (map link joined)
  , axisX: if proper then config.axis.x / axisLength else 0.0
  , axisY: if proper then config.axis.y / axisLength else 1.0
  , iterations: config.iterations
  , hard: config.hard
  , heldX: config.heldX
  , heldY: config.heldY
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

-- | Does this force project its links after the nodes have moved?
isHard :: forall row. Directional row -> Boolean
isHard (Directional force) = force.hard

-- | The most passes the hard projection makes.
passLimit :: Int
passLimit = 32

-- | The hard projection. Nothing unless the force is hard.
constrain :: forall row. Directional row -> Effect Unit
constrain (Directional force) =
  when force.hard do
    moving <- Ref.new true
    forE 0 passLimit \_ -> do
      wanted <- Ref.read moving
      when wanted do
        Ref.write false moving
        foreachE force.links \link ->
          let
            x = link.target.x - link.source.x
            y = link.target.y - link.source.y
            along = x * force.axisX + y * force.axisY
          in
            when
              ( isFinite link.target.x && isFinite link.target.y && isFinite link.source.x && isFinite link.source.y
                  && along < link.gap
              )
              do
                let
                  short = link.gap - along
                  stepX = force.axisX * short
                  stepY = force.axisY * short
                if not (held link.target) then do
                  runEffectFn3 writeVelocity link.target (link.target.vx + stepX) (link.target.vy + stepY)
                  runEffectFn3 writePosition link.target (link.target.x + stepX) (link.target.y + stepY)
                  Ref.write true moving
                else if not (held link.source) then do
                  runEffectFn3 writeVelocity link.source (link.source.vx - stepX) (link.source.vy - stepY)
                  runEffectFn3 writePosition link.source (link.source.x - stepX) (link.source.y - stepY)
                  Ref.write true moving
                else pure unit
  where
  held node = (force.axisX /= 0.0 && force.heldX node) || (force.axisY /= 0.0 && force.heldY node)
