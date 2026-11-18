-- | Rectangle collision: push overlapping axis-aligned rectangles apart
-- | along the axis on which they overlap least.
-- |
-- | Not in d3. The rule is the Rust kernel's (`apply_rect_collide` in
-- | `force-kernel/src/forces.rs`), and the two are held to each other bit
-- | for bit by `test/differential.mjs`.
-- |
-- | Node i is a rectangle of half-width `w_i` and half-height `h_i` centred
-- | on its anticipated position `a_i = position + velocity`. One iteration:
-- |
-- | 1. Take `a_i` for every node, all at once, before anything moves.
-- | 2. For every ordered pair (i, j), j != i, both `a` finite, taking j in
-- |    ascending order for each i:
-- |
-- |    ```
-- |    dx = a_i.x - a_j.x          dy = a_i.y - a_j.y
-- |    ox = (w_i + w_j) - |dx|     oy = (h_i + h_j) - |dy|
-- |    ```
-- |
-- |    The pair overlaps when `ox > 0` and `oy > 0`. If it does:
-- |
-- |    ```
-- |    share = A_j / (A_i + A_j) with A = w * h, or 1/2 if A_i + A_j is not above 0
-- |    if ox <= oy:  v_i.x += s * ox * strength * share
-- |    otherwise:    v_i.y += s * oy * strength * share
-- |    ```
-- |
-- |    where `s` is the sign of dx (or dy): +1 if positive, -1 if negative,
-- |    and if exactly zero, -1 when i < j and +1 when i > j.
-- |
-- | Every correction in an iteration comes from the same anticipated
-- | positions, so no node is favoured by its index. Not scaled by alpha, as
-- | d3's collide is not.
-- |
-- | The quadtree only finds the rectangles that overlap a given one. They
-- | are then put in index order before their corrections are added, which
-- | makes the result the same, bit for bit, as the loop over all pairs.
module Hylograph.Forces.Pure.RectCollide
  ( Config
  , RectCollide
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Function.Uncurried (Fn3, Fn6, Fn7, mkFn3, mkFn6, mkFn7, runFn3, runFn6, runFn7)
import Data.List (List(..), sortBy)
import Data.Number (abs, isFinite, isNaN)
import Effect (Effect, forE, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)
import Hylograph.Forces.Pure.Quadtree (Point, Quadtree(..))
import Hylograph.Forces.Pure.Quadtree as Quadtree

-- | `halfWidth` and `halfHeight` are asked once per node when the force is
-- | initialized; `NaN` counts as zero.
type Config row =
  { halfWidth :: Node row -> Number
  , halfHeight :: Node row -> Number
  , strength :: Number
  , iterations :: Int
  }

-- | A node as the force sees it.
type Box row = { index :: Int, halfWidth :: Number, halfHeight :: Number, node :: Node row }

-- | An initialized rectangle collision.
newtype RectCollide row = RectCollide
  { boxes :: Array (Box row)
  , strength :: Number
  , iterations :: Int
  }

initialize :: forall row. Config row -> Array (Node row) -> RectCollide row
initialize config nodes = RectCollide
  { boxes: mapWithIndex box nodes
  , strength: config.strength
  , iterations: config.iterations
  }
  where
  box index node =
    { index, halfWidth: orZero (config.halfWidth node), halfHeight: orZero (config.halfHeight node), node }

  orZero extent = if isNaN extent then 0.0 else extent

-- | A node's velocity is only ever written by its own turn, so its
-- | anticipated position, read at its turn, is the one the tree was built
-- | from.
apply :: forall row. RectCollide row -> Effect Unit
apply (RectCollide force) =
  forE 0 force.iterations \_ -> do
    points <- anticipate force.boxes
    let chain = cells (Quadtree.build points)
    foreachE force.boxes \box ->
      let
        aheadX = box.node.x + box.node.vx
        aheadY = box.node.y + box.node.vy
      in
        when (isFinite aheadX && isFinite aheadY) do
          case runFn7 overlapping box.index box.halfWidth box.halfHeight aheadX aheadY chain Nil of
            Nil -> pure unit
            found ->
              let
                moved = runFn7 settle force.strength box found aheadX aheadY box.node.vx box.node.vy
              in
                runEffectFn3 writeVelocity box.node moved.vx moved.vy
  where
  settle :: Fn7 Number (Box row) (List (Point (Box row))) Number Number Number Number { vx :: Number, vy :: Number }
  settle = mkFn7 \strength box found aheadX aheadY startVx startVy ->
    let
      area = box.halfWidth * box.halfHeight

      go others vx vy = case others of
        Nil -> { vx, vy }
        Cons other rest ->
          let
            offsetX = aheadX - other.x
            offsetY = aheadY - other.y
            overlapX = (box.halfWidth + other.item.halfWidth) - abs offsetX
            overlapY = (box.halfHeight + other.item.halfHeight) - abs offsetY
            otherArea = other.item.halfWidth * other.item.halfHeight
            share = if area + otherArea > 0.0 then otherArea / (area + otherArea) else 0.5
            tie = if box.index < other.item.index then -1.0 else 1.0
          in
            if overlapX <= overlapY then
              go rest (vx + runFn3 direction offsetX tie overlapX * strength * share) vy
            else go rest vx (vy + runFn3 direction offsetY tie overlapY * strength * share)
    in
      go (inIndexOrder found) startVx startVy

-- | `s * overlap`: the overlap, signed by which side the other rectangle
-- | is on, the tie deciding when the centres are level.
direction :: Fn3 Number Number Number Number
direction = mkFn3 \offset tie overlap ->
  if offset > 0.0 then 1.0 * overlap
  else if offset < 0.0 then -1.0 * overlap
  else tie * overlap

inIndexOrder :: forall row. List (Point (Box row)) -> List (Point (Box row))
inIndexOrder found = case found of
  Cons _ Nil -> found
  _ -> sortBy (comparing _.item.index) found

-- | Where each node will be once its velocity is added, newest first. An
-- | effect, because the nodes are mutable.
anticipate :: forall row. Array (Box row) -> Effect (List (Point (Box row)))
anticipate boxes = do
  points <- Ref.new Nil
  foreachE boxes \box -> do
    earlier <- Ref.read points
    Ref.write (Cons { x: box.node.x + box.node.vx, y: box.node.y + box.node.vy, item: box } earlier) points
  Ref.read points

-- | One square of the tree, ready for the walk. The fields are, in order:
-- | the largest half-width and the largest half-height of any rectangle
-- | inside; the square's corners x0, y0, x1, y1; where the walk goes to open
-- | the square; where it goes when the square is done; and the rectangles
-- | of a leaf (none for a square with quarters).
data Cell row
  = Cell Number Number Number Number Number Number (Cell row) (Cell row) (List (Point (Box row)))
  | Finished

-- | The whole tree as a chain of cells.
cells :: forall row. Quadtree.Planted (Box row) -> Cell row
cells planted =
  runFn6 thread planted.root planted.extent.x0 planted.extent.y0 planted.extent.x1 planted.extent.y1 Finished
  where
  thread :: Fn6 (Quadtree (Box row)) Number Number Number Number (Cell row) (Cell row)
  thread = mkFn6 \tree x0 y0 x1 y1 after -> case tree of
    Empty -> after
    Leaf newest older ->
      let
        occupants = Cons newest older
      in
        Cell (widest occupants 0.0) (tallest occupants 0.0) x0 y0 x1 y1 after after occupants
    Branch topLeft topRight bottomLeft bottomRight ->
      let
        middleX = (x0 + x1) / 2.0
        middleY = (y0 + y1) / 2.0
        fourth = runFn6 thread bottomRight middleX middleY x1 y1 after
        third = runFn6 thread bottomLeft x0 middleY middleX y1 fourth
        second = runFn6 thread topRight middleX y0 x1 middleY third
        first = runFn6 thread topLeft x0 y0 middleX middleY second
        width =
          runFn3 wider bottomRight fourth
            (runFn3 wider bottomLeft third (runFn3 wider topRight second (runFn3 wider topLeft first 0.0)))
        height =
          runFn3 taller bottomRight fourth
            (runFn3 taller bottomLeft third (runFn3 taller topRight second (runFn3 taller topLeft first 0.0)))
      in
        Cell width height x0 y0 x1 y1 first after Nil

  widest :: List (Point (Box row)) -> Number -> Number
  widest occupants most = case occupants of
    Nil -> most
    Cons point rest -> widest rest (if point.item.halfWidth > most then point.item.halfWidth else most)

  tallest :: List (Point (Box row)) -> Number -> Number
  tallest occupants most = case occupants of
    Nil -> most
    Cons point rest -> tallest rest (if point.item.halfHeight > most then point.item.halfHeight else most)

  wider :: Fn3 (Quadtree (Box row)) (Cell row) Number Number
  wider = mkFn3 \quarter cell most -> case quarter of
    Empty -> most
    _ -> case cell of
      Cell width _ _ _ _ _ _ _ _ -> if width > most then width else most
      Finished -> most

  taller :: Fn3 (Quadtree (Box row)) (Cell row) Number Number
  taller = mkFn3 \quarter cell most -> case quarter of
    Empty -> most
    _ -> case cell of
      Cell _ height _ _ _ _ _ _ _ -> if height > most then height else most
      Finished -> most

-- | Every rectangle that overlaps the walking one (index, half-width,
-- | half-height, anticipated position), added to the front of `found`. A
-- | square is passed over only when nothing in it can reach.
overlapping
  :: forall row
   . Fn7 Int Number Number Number Number (Cell row) (List (Point (Box row))) (List (Point (Box row)))
overlapping = mkFn7 \index halfWidth halfHeight aheadX aheadY chain start ->
  let
    go cell found = case cell of
      Cell width height x0 y0 x1 y1 inside after occupants -> case occupants of
        Nil ->
          if
            x0 - aheadX > halfWidth + width || aheadX - x1 > halfWidth + width
              || y0 - aheadY > halfHeight + height
              || aheadY - y1 > halfHeight + height then go after found
          else go inside found
        _ -> go after (runFn7 among index halfWidth halfHeight aheadX aheadY occupants found)
      Finished -> found
  in
    go chain start

-- | The rectangles of one leaf that overlap the walking one.
among
  :: forall row
   . Fn7 Int Number Number Number Number (List (Point (Box row))) (List (Point (Box row))) (List (Point (Box row)))
among = mkFn7 \index halfWidth halfHeight aheadX aheadY occupants start ->
  let
    go remaining found = case remaining of
      Nil -> found
      Cons other rest ->
        if
          other.item.index /= index
            && (halfWidth + other.item.halfWidth) - abs (aheadX - other.x) > 0.0
            && (halfHeight + other.item.halfHeight) - abs (aheadY - other.y) > 0.0 then go rest (Cons other found)
        else go rest found
  in
    go occupants start
