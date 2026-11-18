-- | Containment: keep each node inside a region. A soft force steers a node
-- | back in before the nodes move; an optional hard confine puts it back in
-- | after they have.
-- |
-- | Not in d3. The rules are the Rust kernel's (`Region::project`,
-- | `apply_contain` and `constrain` in `force-kernel/src/forces.rs`), and the
-- | two are held to each other bit for bit by `test/differential.mjs`.
-- |
-- | **Projection.** `project region radius x y` is where a circle of that
-- | radius centred on (x, y) has to move its centre to, to lie inside the
-- | region. A centre already far enough inside comes back unchanged.
-- |
-- | - Rectangle: confine x to `[x0 + radius, x1 - radius]` and y likewise. If
-- |   the rectangle is narrower than the circle on an axis, that coordinate
-- |   goes to the middle of the rectangle.
-- | - Circle: with `R' = max(R - radius, 0)`, a centre further than `R'`
-- |   from the region's centre `c` moves to `c + (p - c) * (R' / |p - c|)`.
-- | - Convex polygon: one pass visits the edges in order; for each, with `d`
-- |   the signed distance from the centre (as it now is) to the edge's line,
-- |   positive inside, a centre with `d < radius` moves along the inward
-- |   normal by `radius - d`. Passes repeat until one moves nothing, eight
-- |   at most.
-- |
-- | **Soft.** For every node with a region, with `a = position + velocity`
-- | finite and `c = project region radius a`: `v += (c - a) * strength`. Not
-- | scaled by alpha: it is a constraint, like collide.
-- |
-- | **Hard.** After the nodes have moved, every node with a region and a
-- | finite position is put at `project region radius position`, and its
-- | velocity changes by the same amount, so that the velocity is still the
-- | step the node actually took. Pinned coordinates are then put back: a
-- | pin beats a region.
module Hylograph.Forces.Pure.Contain
  ( Region
  , rectangle
  , circle
  , polygon
  , project
  , Confinement
  , free
  , within
  , Config
  , Contain
  , initialize
  , apply
  , isHard
  , constrain
  ) where

import Prelude

import Data.Array (catMaybes, drop, length, take, zipWith)
import Data.Foldable (foldl)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite, isNaN, sqrt)
import Effect (Effect, foreachE)
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, writePosition, writeVelocity)

-- | A region nodes can be confined to. A polygon is held as its edges: a
-- | point on each and the edge's inward unit normal, in the order the
-- | vertices were given.
data Region
  = Rectangle { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
  | Circle { x :: Number, y :: Number, radius :: Number }
  | Polygon (List Edge)

type Edge = { x :: Number, y :: Number, normalX :: Number, normalY :: Number }

-- | The rectangle with these two corners, in either order.
rectangle :: Number -> Number -> Number -> Number -> Region
rectangle x0 y0 x1 y1 = Rectangle
  { x0: if x1 < x0 then x1 else x0
  , y0: if y1 < y0 then y1 else y0
  , x1: if x1 < x0 then x0 else x1
  , y1: if y1 < y0 then y0 else y1
  }

-- | The circle about (x, y).
circle :: Number -> Number -> Number -> Region
circle x y radius = Circle { x, y, radius }

-- | A convex polygon from its vertices, in either winding. Convexity is the
-- | caller's promise and is not checked. Fewer than three vertices, or zero
-- | area, gives a polygon with no edges, which confines nothing. An edge of
-- | zero length is left out.
polygon :: Array { x :: Number, y :: Number } -> Region
polygon vertices = Polygon (fromFoldable edges)
  where
  count = length vertices
  -- Each vertex with the one after it, the last with the first.
  sides = zipWith (\from to -> { from, to }) vertices (drop 1 vertices <> take 1 vertices)
  doubleArea = foldl (\area side -> area + (side.from.x * side.to.y - side.to.x * side.from.y)) 0.0 sides
  -- Counter-clockwise (positive area): inward is to the left of the edge.
  sign = if doubleArea > 0.0 then 1.0 else -1.0

  edges =
    if count >= 3 && doubleArea /= 0.0 && isFinite doubleArea then catMaybes (map edge sides)
    else []

  edge side =
    let
      alongX = side.to.x - side.from.x
      alongY = side.to.y - side.from.y
      sideLength = sqrt (alongX * alongX + alongY * alongY)
    in
      if sideLength > 0.0 then
        Just { x: side.from.x, y: side.from.y, normalX: -alongY / sideLength * sign, normalY: alongX / sideLength * sign }
      else Nothing

-- | Where a circle of the given radius centred on (x, y) has to move its
-- | centre to, to lie inside the region.
project :: Region -> Number -> Number -> Number -> { x :: Number, y :: Number }
project region radius x y = case region of
  Rectangle box ->
    { x: confine radius box.x0 box.x1 x, y: confine radius box.y0 box.y1 y }
  Circle disc ->
    let
      inner = if disc.radius - radius > 0.0 then disc.radius - radius else 0.0
      offsetX = x - disc.x
      offsetY = y - disc.y
      distance2 = offsetX * offsetX + offsetY * offsetY
    in
      if distance2 > inner * inner then
        let
          scale = inner / sqrt distance2
        in
          { x: disc.x + offsetX * scale, y: disc.y + offsetY * scale }
      else { x, y }
  Polygon edges -> passes edges radius 8 x y

-- | One coordinate into `[low + radius, high - radius]`, or to the middle
-- | where that interval is empty.
confine :: Number -> Number -> Number -> Number -> Number
confine radius low high value =
  let
    least = low + radius
    most = high - radius
  in
    if least > most then (low + high) / 2.0
    else if value < least then least
    else if value > most then most
    else value

-- | Up to `remaining` passes over a polygon's edges, stopping after the
-- | first that moves nothing.
passes :: List Edge -> Number -> Int -> Number -> Number -> { x :: Number, y :: Number }
passes edges radius remaining x y =
  if remaining <= 0 then { x, y }
  else
    let
      after = pass edges radius x y false
    in
      if after.moved then passes edges radius (remaining - 1) after.x after.y
      else { x: after.x, y: after.y }

-- | One pass: each edge in turn pushes the centre inward if it is closer to
-- | that edge than the radius.
pass :: List Edge -> Number -> Number -> Number -> Boolean -> { x :: Number, y :: Number, moved :: Boolean }
pass edges radius x y moved = case edges of
  Nil -> { x, y, moved }
  Cons edge rest ->
    let
      inside = (x - edge.x) * edge.normalX + (y - edge.y) * edge.normalY
    in
      if inside < radius then
        pass rest radius (x + edge.normalX * (radius - inside)) (y + edge.normalY * (radius - inside)) true
      else pass rest radius x y moved

-- | Whether a node is confined, and to what.
data Confinement
  = Free
  | Within Region

-- | Not confined.
free :: Confinement
free = Free

-- | Confined to this region.
within :: Region -> Confinement
within = Within

-- | `region` and `radius` are asked once per node when the force is
-- | initialized; a radius of `NaN` counts as zero. `strength` is the soft
-- | force's (zero turns it off); `hard` adds the confine after the nodes move.
type Config row =
  { region :: Node row -> Confinement
  , radius :: Node row -> Number
  , strength :: Number
  , hard :: Boolean
  }

-- | A confined node.
type Kept row = { node :: Node row, region :: Region, radius :: Number }

-- | An initialized containment.
newtype Contain row = Contain { kept :: Array (Kept row), strength :: Number, hard :: Boolean }

initialize :: forall row. Config row -> Array (Node row) -> Contain row
initialize config nodes = Contain
  { kept: catMaybes (map kept nodes)
  , strength: config.strength
  , hard: config.hard
  }
  where
  kept node = case config.region node of
    Free -> Nothing
    Within region ->
      let
        radius = config.radius node
      in
        Just { node, region, radius: if isNaN radius then 0.0 else radius }

-- | The soft part, applied with the forces.
apply :: forall row. Contain row -> Effect Unit
apply (Contain force) =
  foreachE force.kept \kept ->
    let
      aheadX = kept.node.x + kept.node.vx
      aheadY = kept.node.y + kept.node.vy
    in
      when (isFinite aheadX && isFinite aheadY) do
        let allowed = project kept.region kept.radius aheadX aheadY
        runEffectFn3 writeVelocity kept.node
          (kept.node.vx + (allowed.x - aheadX) * force.strength)
          (kept.node.vy + (allowed.y - aheadY) * force.strength)

-- | Whether this containment clamps after the nodes move.
isHard :: forall row. Contain row -> Boolean
isHard (Contain force) = force.hard

-- | The hard part, applied after the nodes have moved; nothing unless the
-- | containment is hard. The velocity is written first, from the position
-- | as it still is, and then the position.
constrain :: forall row. Contain row -> Effect Unit
constrain (Contain force) =
  when force.hard do
    foreachE force.kept \kept ->
      when (isFinite kept.node.x && isFinite kept.node.y) do
        let allowed = project kept.region kept.radius kept.node.x kept.node.y
        runEffectFn3 writeVelocity kept.node
          (kept.node.vx + (allowed.x - kept.node.x))
          (kept.node.vy + (allowed.y - kept.node.y))
        runEffectFn3 writePosition kept.node allowed.x allowed.y
