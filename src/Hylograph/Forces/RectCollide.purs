-- | Rectangle collision: push overlapping axis-aligned rectangles apart
-- | along the axis on which they overlap least.
-- |
-- | Not in d3. The rule is the Rust kernel's (`apply_rect_collide` in
-- | `force-kernel/src/forces.rs`), and the two are held to each other bit
-- | for bit by `test/differential.mjs`.
-- |
-- | Node i is a rectangle of half-width `w_i` and half-height `h_i` centred
-- | on its anticipated position `a_i = position + velocity`. A node is HELD
-- | on an axis if it cannot be moved along it: it is pinned there (`fx` for
-- | x, `fy` for y; here, whatever the config's `heldX` and `heldY` say). One
-- | iteration:
-- |
-- | 1. Take `a_i` for every node that takes part, all at once, before
-- |    anything moves.
-- | 2. For every node i that takes part, and every other node j that takes
-- |    part and may collide with it (see "Who collides"), both `a` finite,
-- |    taking j in ascending order:
-- |
-- |    ```
-- |    dx = a_i.x - a_j.x          dy = a_i.y - a_j.y
-- |    ox = (w_i + w_j) - |dx|     oy = (h_i + h_j) - |dy|
-- |    ```
-- |
-- |    The pair overlaps when `ox > 0` and `oy > 0`. If it does, the push is
-- |    along x if `ox <= oy` and along y otherwise. If i is held on that
-- |    axis, nothing more happens for this pair. Otherwise
-- |
-- |    ```
-- |    share = 1                      if j is held on that axis
-- |          = A_j / (A_i + A_j)      otherwise, with A = w * h
-- |                                   (1/2 if A_i + A_j is not above 0)
-- |    push  = s * overlap * strength * share
-- |    v_i[axis] += push
-- |    ```
-- |
-- |    where `s` is the sign of dx (or dy): +1 if positive, -1 if negative,
-- |    and if exactly zero, -1 when i < j and +1 when i > j.
-- |
-- | So a held rectangle is never moved, and what overlaps it takes the whole
-- | correction; two free rectangles share it, the smaller moving more.
-- |
-- | THE SQUEEZE. A wide rectangle nearly always overlaps another least
-- | vertically, so it is only ever pushed up or down; caught between two
-- | things that do not give way (two pinned dots, a dot and a packed row of
-- | other labels) the pushes cancel and it sits on both for ever. With
-- | `squeezed: goesThrough`, after the last iteration of one application of
-- | the force, for each node i and each axis it is not held on: let `up` be
-- | the sum of the positive pushes step 2 gave it on that axis over all the
-- | iterations, and `down` the sum of the negative ones, as a magnitude. If
-- | both are above 0 the node is squeezed, and it goes with one side and
-- | ignores the other:
-- |
-- | ```
-- | v_i[axis] += down     if it goes up     (the downward pushes are undone)
-- | v_i[axis] -= up       if it goes down
-- | ```
-- |
-- | It goes the way it was already moving, if it was moving: with `w` its
-- | velocity on that axis as it was when the force was applied, if
-- | `|w| > 0.001 * (up + down)` it goes up when `w > 0` and down when
-- | `w < 0`. Otherwise (a velocity that small is what is left of a movement
-- | long over, or the last of a pull dying away) it goes with the stronger
-- | side: up if `up > down`, down if `up < down`, and if they are equal, up
-- | when i is odd and down when it is even.
-- |
-- | Going the way it is moving is what makes this resolve: the rectangle
-- | keeps on through whatever was squeezing it, a dot or another rectangle,
-- | until it is clear, where a rule that only compared the two sides would
-- | turn round as soon as it went deeper into the weaker one. It suits
-- | rectangles that nothing else is pressing together. Under a force that
-- | keeps pressing (a pull to a point that a pinned body is standing on) a
-- | squeeze is permanent and the rectangle will be walked through its
-- | neighbour, so let such a pull die away before expecting rest. With
-- | `squeezed: staysPut`, the default, step 2 is all there is.
-- |
-- | Who collides. A node left out by `include` collides with nothing and
-- | nothing collides with it. With groups, `pairs` restricts the pairs: only
-- | two nodes of the same group (`withinGroups`), or only two nodes not of
-- | the same group (`betweenGroups`; a node in no group is in a different
-- | group from everyone).
-- |
-- | Every correction in an iteration comes from the same anticipated
-- | positions, so no node is favoured by its index. Not scaled by alpha, as
-- | d3's collide is not.
-- |
-- | The quadtree only finds the rectangles that overlap a given one. They
-- | are then put in index order before their corrections are added, which
-- | makes the result the same, bit for bit, as the loop over all pairs.
module Hylograph.Forces.RectCollide
  ( Config
  , defaultConfig
  , Pairs
  , everyPair
  , withinGroups
  , betweenGroups
  , Squeezed
  , staysPut
  , goesThrough
  , RectCollide
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (filter, mapWithIndex)
import Data.Function.Uncurried (Fn3, Fn6, Fn7, Fn8, mkFn3, mkFn6, mkFn7, mkFn8, runFn3, runFn6, runFn7, runFn8)
import Data.Int (odd)
import Data.List (List(..), reverse, sortBy)
import Data.Number (abs, isFinite, isNaN)
import Effect (Effect, forE, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Group (Membership(..))
import Hylograph.Forces.Node (Node, writeVelocity)
import Hylograph.Forces.Quadtree (Point, Quadtree(..))
import Hylograph.Forces.Quadtree as Quadtree

-- | Which pairs of rectangles collide.
data Pairs
  = EveryPair
  | WithinGroups
  | BetweenGroups

everyPair :: Pairs
everyPair = EveryPair

withinGroups :: Pairs
withinGroups = WithinGroups

betweenGroups :: Pairs
betweenGroups = BetweenGroups

-- | What a rectangle pushed from both sides does.
data Squeezed
  = StaysPut
  | GoesThrough

staysPut :: Squeezed
staysPut = StaysPut

goesThrough :: Squeezed
goesThrough = GoesThrough

-- | `halfWidth`, `halfHeight`, `include` and `group` are asked once per node
-- | when the force is initialized; a half-extent of `NaN` counts as zero.
-- | `heldX` and `heldY` say whether a node cannot be moved along an axis,
-- | and are asked every time the force is applied, since pins come and go:
-- | `Node.pinnedX` and `Node.pinnedY` are the ones to give for nodes with
-- | `fx` and `fy`.
type Config row =
  { halfWidth :: Node row -> Number
  , halfHeight :: Node row -> Number
  , strength :: Number
  , iterations :: Int
  , include :: Node row -> Boolean
  , pairs :: Pairs
  , group :: Node row -> Membership
  , heldX :: Node row -> Boolean
  , heldY :: Node row -> Boolean
  , squeezed :: Squeezed
  }

-- | Unit squares, full strength, one iteration, every node, every pair,
-- | nothing held, a squeezed rectangle stays put.
defaultConfig :: forall row. Config row
defaultConfig =
  { halfWidth: \_ -> 1.0
  , halfHeight: \_ -> 1.0
  , strength: 1.0
  , iterations: 1
  , include: \_ -> true
  , pairs: EveryPair
  , group: \_ -> Ungrouped
  , heldX: \_ -> false
  , heldY: \_ -> false
  , squeezed: StaysPut
  }

-- | A node as the force sees it. `group` is its group's number, or -1.
type Box row = { index :: Int, halfWidth :: Number, halfHeight :: Number, group :: Int, node :: Node row }

-- | An initialized rectangle collision: the nodes that take part.
newtype RectCollide row = RectCollide
  { boxes :: Array (Box row)
  , strength :: Number
  , iterations :: Int
  , pairs :: Pairs
  , heldX :: Node row -> Boolean
  , heldY :: Node row -> Boolean
  , squeezed :: Squeezed
  }

initialize :: forall row. Config row -> Array (Node row) -> RectCollide row
initialize config nodes = RectCollide
  { boxes: filter (\candidate -> config.include candidate.node) (mapWithIndex box nodes)
  , strength: config.strength
  , iterations: config.iterations
  , pairs: config.pairs
  , heldX: config.heldX
  , heldY: config.heldY
  , squeezed: config.squeezed
  }
  where
  box index node =
    { index
    , halfWidth: orZero (config.halfWidth node)
    , halfHeight: orZero (config.halfHeight node)
    , group: case config.group node of
        Ungrouped -> -1
        InGroup number -> number
    , node
    }

  orZero extent = if isNaN extent then 0.0 else extent

-- | What step 2 has given one rectangle so far in this application: its
-- | velocity when the force was applied, and on each axis the sum of the
-- | pushes one way and of the pushes the other.
type Pushed = { startVx :: Number, startVy :: Number, upX :: Number, downX :: Number, upY :: Number, downY :: Number }

-- | A node's velocity is only ever written by its own turn, so its
-- | anticipated position, read at its turn, is the one the tree was built
-- | from.
apply :: forall row. RectCollide row -> Effect Unit
apply (RectCollide force) = case force.squeezed of
  StaysPut ->
    forE 0 force.iterations \_ -> do
      points <- anticipate force.boxes
      let chain = cells (Quadtree.build points)
      foreachE force.boxes \box ->
        let
          aheadX = box.node.x + box.node.vx
          aheadY = box.node.y + box.node.vy
        in
          when (isFinite aheadX && isFinite aheadY) do
            case runFn7 overlapping (partners box) box.halfWidth box.halfHeight aheadX aheadY chain Nil of
              Nil -> pure unit
              found ->
                let
                  moved = runFn8 settle force.strength box found aheadX aheadY box.node.vx box.node.vy unpushed
                in
                  runEffectFn3 writeVelocity box.node moved.vx moved.vy
  GoesThrough -> do
    -- What has pushed each rectangle, in the order of the boxes, carried
    -- from one iteration to the next.
    start <- starting force.boxes
    pushed <- Ref.new start
    forE 0 force.iterations \_ -> do
      points <- anticipate force.boxes
      let chain = cells (Quadtree.build points)
      before <- Ref.read pushed
      waiting <- Ref.new before
      done <- Ref.new Nil
      foreachE force.boxes \box -> do
        queue <- Ref.read waiting
        case queue of
          Nil -> pure unit
          Cons soFar rest -> do
            Ref.write rest waiting
            let
              aheadX = box.node.x + box.node.vx
              aheadY = box.node.y + box.node.vy
            if isFinite aheadX && isFinite aheadY then
              case runFn7 overlapping (partners box) box.halfWidth box.halfHeight aheadX aheadY chain Nil of
                Nil -> Ref.modify_ (Cons soFar) done
                found -> do
                  let moved = runFn8 settle force.strength box found aheadX aheadY box.node.vx box.node.vy soFar
                  runEffectFn3 writeVelocity box.node moved.vx moved.vy
                  Ref.modify_ (Cons moved.pushed) done
            else Ref.modify_ (Cons soFar) done
      newestFirst <- Ref.read done
      Ref.write (reverse newestFirst) pushed
    -- The squeeze.
    sums <- Ref.read pushed
    remaining <- Ref.new sums
    foreachE force.boxes \box -> do
      queue <- Ref.read remaining
      case queue of
        Nil -> pure unit
        Cons soFar rest -> do
          Ref.write rest remaining
          let
            throughX = through box.index soFar.startVx soFar.upX soFar.downX
            throughY = through box.index soFar.startVy soFar.upY soFar.downY
          when (throughX /= 0.0 || throughY /= 0.0) do
            runEffectFn3 writeVelocity box.node (box.node.vx + throughX) (box.node.vy + throughY)
  where
  partners box other = other.index /= box.index && may force.pairs box other

  unpushed = { startVx: 0.0, startVy: 0.0, upX: 0.0, downX: 0.0, upY: 0.0, downY: 0.0 }

  -- The velocity of every box now, with nothing pushed yet. An effect,
  -- because the nodes are mutable.
  starting boxes = do
    gathered <- Ref.new Nil
    foreachE boxes \box ->
      Ref.modify_ (Cons { startVx: box.node.vx, startVy: box.node.vy, upX: 0.0, downX: 0.0, upY: 0.0, downY: 0.0 }) gathered
    newestFirst <- Ref.read gathered
    pure (reverse newestFirst)

  -- Step 2 of the rule, for one rectangle and the ones it overlaps. A held
  -- axis is never pushed, so its sums stay zero and it is never squeezed.
  settle :: Fn8 Number (Box row) (List (Point (Box row))) Number Number Number Number Pushed { vx :: Number, vy :: Number, pushed :: Pushed }
  settle = mkFn8 \strength box found aheadX aheadY startVx startVy soFar ->
    let
      area = box.halfWidth * box.halfHeight
      heldX = force.heldX box.node
      heldY = force.heldY box.node

      go others vx vy upX downX upY downY = case others of
        Nil -> { vx, vy, pushed: { startVx: soFar.startVx, startVy: soFar.startVy, upX, downX, upY, downY } }
        Cons other rest ->
          let
            offsetX = aheadX - other.x
            offsetY = aheadY - other.y
            overlapX = (box.halfWidth + other.item.halfWidth) - abs offsetX
            overlapY = (box.halfHeight + other.item.halfHeight) - abs offsetY
            otherArea = other.item.halfWidth * other.item.halfHeight
            byArea = if area + otherArea > 0.0 then otherArea / (area + otherArea) else 0.5
            tie = if box.index < other.item.index then -1.0 else 1.0
          in
            if overlapX <= overlapY then
              if heldX then go rest vx vy upX downX upY downY
              else
                let
                  push = runFn3 direction offsetX tie overlapX * strength * (if force.heldX other.item.node then 1.0 else byArea)
                in
                  if push > 0.0 then go rest (vx + push) vy (upX + push) downX upY downY
                  else if push < 0.0 then go rest (vx + push) vy upX (downX - push) upY downY
                  else go rest (vx + push) vy upX downX upY downY
            else if heldY then go rest vx vy upX downX upY downY
            else
              let
                push = runFn3 direction offsetY tie overlapY * strength * (if force.heldY other.item.node then 1.0 else byArea)
              in
                if push > 0.0 then go rest vx (vy + push) upX downX (upY + push) downY
                else if push < 0.0 then go rest vx (vy + push) upX downX upY (downY - push)
                else go rest vx (vy + push) upX downX upY downY
    in
      go (inIndexOrder found) startVx startVy soFar.upX soFar.downX soFar.upY soFar.downY

  -- What a squeezed rectangle adds to its velocity on one axis: the pushes
  -- of the side it ignores, undone. Nothing if it was not squeezed.
  through index startVelocity up down =
    if up > 0.0 && down > 0.0 then
      if abs startVelocity > 0.001 * (up + down) then
        if startVelocity > 0.0 then down else -up
      else if up > down then down
      else if up < down then -up
      else if odd index then down
      else -up
    else 0.0

-- | May this rectangle collide with that one, by the groups they are in?
may :: forall row. Pairs -> Box row -> Box row -> Boolean
may pairs box other = case pairs of
  EveryPair -> true
  WithinGroups -> box.group >= 0 && box.group == other.group
  BetweenGroups -> not (box.group >= 0 && box.group == other.group)

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

-- | Every rectangle that may collide with the walking one and overlaps it
-- | (which ones may, then its half-width, half-height and anticipated
-- | position), added to the front of `found`. A
-- | square is passed over only when nothing in it can reach.
overlapping
  :: forall row
   . Fn7 (Box row -> Boolean) Number Number Number Number (Cell row) (List (Point (Box row))) (List (Point (Box row)))
overlapping = mkFn7 \allowed halfWidth halfHeight aheadX aheadY chain start ->
  let
    go cell found = case cell of
      Cell width height x0 y0 x1 y1 inside after occupants -> case occupants of
        Nil ->
          if
            x0 - aheadX > halfWidth + width || aheadX - x1 > halfWidth + width
              || y0 - aheadY > halfHeight + height
              || aheadY - y1 > halfHeight + height then go after found
          else go inside found
        _ -> go after (runFn7 among allowed halfWidth halfHeight aheadX aheadY occupants found)
      Finished -> found
  in
    go chain start

-- | The rectangles of one leaf that overlap the walking one.
among
  :: forall row
   . Fn7 (Box row -> Boolean) Number Number Number Number (List (Point (Box row))) (List (Point (Box row))) (List (Point (Box row)))
among = mkFn7 \allowed halfWidth halfHeight aheadX aheadY occupants start ->
  let
    go remaining found = case remaining of
      Nil -> found
      Cons other rest ->
        if
          allowed other.item
            && (halfWidth + other.item.halfWidth) - abs (aheadX - other.x) > 0.0
            && (halfHeight + other.item.halfHeight) - abs (aheadY - other.y) > 0.0 then go rest (Cons other found)
        else go rest found
  in
    go occupants start
