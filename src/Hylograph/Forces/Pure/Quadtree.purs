-- | The part of d3-quadtree that d3-force uses: build a tree from a set of
-- | points, once, and hand it to a force to walk.
-- |
-- | Ported from d3-quadtree 3.0.1 (ISC, Copyright 2010-2021 Mike Bostock);
-- | see LICENSE.
-- |
-- | d3 inserts the points one at a time into a mutable tree. This builds the
-- | same tree from the top down, without mutation: a square that holds no
-- | point is `Empty`, one that holds a single location is a `Leaf`, and any
-- | other is a `Branch` of its four quarters. That is the tree d3's insertion
-- | arrives at whatever the order, because d3 splits a square exactly when a
-- | second location lands in it. Two things do depend on insertion order and
-- | are kept: the points of a leaf run newest first, as d3's `next` chain
-- | does, and the extent starts from the unit square at the floor of the
-- | smallest coordinates and doubles until it covers the largest.
-- |
-- | The squares themselves are not stored. A force recomputes them as it
-- | walks, halving from the root's extent with the same `(x0 + x1) / 2` that
-- | d3's `visit` uses, so the bounds agree to the bit.
module Hylograph.Forces.Pure.Quadtree
  ( Point
  , Quadtree(..)
  , Extent
  , Planted
  , build
  ) where

import Prelude

import Data.Function.Uncurried (Fn6, mkFn6, runFn6)
import Data.List (List(..), reverse)
import Data.Number (floor, infinity, isNaN)

-- | A location and whatever the force keeps there.
type Point item = { x :: Number, y :: Number, item :: item }

-- | A quadtree. The quarters of a `Branch` are in d3's order: top-left,
-- | top-right, bottom-left, bottom-right, where "right" is `x >= middle` and
-- | "bottom" is `y >= middle`. A `Leaf` holds its newest point and then the
-- | older points at the same location, newest first.
data Quadtree item
  = Empty
  | Leaf (Point item) (List (Point item))
  | Branch (Quadtree item) (Quadtree item) (Quadtree item) (Quadtree item)

-- | The square a tree covers.
type Extent = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

-- | A tree and the square its root covers.
type Planted item = { extent :: Extent, root :: Quadtree item }

-- | d3's `quadtree(nodes, x, y)`: the tree of the given points. They are
-- | listed newest first, that is, in the reverse of the order d3 would insert
-- | them, which is how a list comes out when it is built by adding each node
-- | to the front. Points with a `NaN` coordinate are left out, as d3 leaves
-- | them out.
build :: forall item. List (Point item) -> Planted item
build newestFirst = survey newestFirst Nil infinity infinity (-infinity) (-infinity)
  where
  -- d3's `addAll`, first half: the extent of the valid points. The points
  -- that are kept come out oldest first.
  survey remaining kept x0 y0 x1 y1 = case remaining of
    Nil ->
      if x0 > x1 || y0 > y1 then { extent: { x0, y0, x1, y1 }, root: Empty }
      else
        let
          extent = cover (unitSquare x0 y0) x1 y1
        in
          { extent, root: runFn6 cell false extent.x0 extent.y0 extent.x1 extent.y1 kept }
    Cons point rest ->
      if isNaN point.x || isNaN point.y then survey rest kept x0 y0 x1 y1
      else survey rest (Cons point kept)
        (if point.x < x0 then point.x else x0)
        (if point.y < y0 then point.y else y0)
        (if point.x > x1 then point.x else x1)
        (if point.y > y1 then point.y else y1)

-- | d3's `cover` on a tree with no extent yet: the unit square at the floor
-- | of the point. Integer corners, so that doubling never moves a boundary.
unitSquare :: Number -> Number -> Extent
unitSquare x y =
  let
    x0 = floor x
    y0 = floor y
  in
    { x0, y0, x1: x0 + 1.0, y1: y0 + 1.0 }

-- | d3's `cover` on a tree that has an extent: double the square, towards
-- | the point, until the point is inside.
cover :: Extent -> Number -> Number -> Extent
cover extent x y = grow extent.x0 extent.y0 extent.x1 extent.y1 side
  where
  width = extent.x1 - extent.x0
  -- d3: `z = x1 - x0 || 1`
  side = if width < 0.0 || width > 0.0 then width else 1.0

  grow x0 y0 x1 y1 size =
    if x0 > x || x >= x1 || y0 > y || y >= y1 then
      let
        doubled = size * 2.0
      in
        if y < y0 then
          if x < x0 then grow (x1 - doubled) (y1 - doubled) x1 y1 doubled
          else grow x0 (y1 - doubled) (x0 + doubled) y1 doubled
        else
          if x < x0 then grow (x1 - doubled) y0 x1 (y0 + doubled) doubled
          else grow x0 y0 (x0 + doubled) (y0 + doubled) doubled
    else { x0, y0, x1, y1 }

-- | The tree of the points that fall in one square: whether its list runs
-- | newest first, its corners, and the list. Dealing points into quarters
-- | reverses them, so the direction alternates with depth and a leaf may
-- | have to turn its list round. (An `Fn6`, so that the recursion does not
-- | build six closures per square.)
cell :: forall item. Fn6 Boolean Number Number Number Number (List (Point item)) (Quadtree item)
cell = mkFn6 \newestFirst x0 y0 x1 y1 points ->
  let
    middleX = (x0 + x1) / 2.0
    middleY = (y0 + y1) / 2.0
    turned = not newestFirst

    deal remaining topLeft topRight bottomLeft bottomRight = case remaining of
      Nil ->
        Branch
          (runFn6 cell turned x0 y0 middleX middleY topLeft)
          (runFn6 cell turned middleX y0 x1 middleY topRight)
          (runFn6 cell turned x0 middleY middleX y1 bottomLeft)
          (runFn6 cell turned middleX middleY x1 y1 bottomRight)
      Cons point rest ->
        if point.y >= middleY then
          if point.x >= middleX then deal rest topLeft topRight bottomLeft (Cons point bottomRight)
          else deal rest topLeft topRight (Cons point bottomLeft) bottomRight
        else
          if point.x >= middleX then deal rest topLeft (Cons point topRight) bottomLeft bottomRight
          else deal rest (Cons point topLeft) topRight bottomLeft bottomRight
  in
    case points of
      Nil -> Empty
      Cons first rest ->
        if coincident first rest then leaf newestFirst first rest
        else deal points Nil Nil Nil Nil

-- | Whether every point of the list is at the first point's location: d3's
-- | `x === xp && y === yp`.
coincident :: forall item. Point item -> List (Point item) -> Boolean
coincident first others = case elsewhere first others of
  Nil -> true
  _ -> false

-- | The list from its first point that is not at the given point's location.
-- | (It returns the list rather than a `Boolean` so that the recursion is a
-- | plain tail call under both backends: the optimising backend folds a
-- | recursive `if ... then ... else false` into `&&`, and then no longer
-- | turns it into a loop.)
elsewhere :: forall item. Point item -> List (Point item) -> List (Point item)
elsewhere first others = case others of
  Nil -> Nil
  Cons other rest ->
    if other.x == first.x && other.y == first.y then elsewhere first rest
    else others

-- | A leaf of coincident points, newest first.
leaf :: forall item. Boolean -> Point item -> List (Point item) -> Quadtree item
leaf newestFirst first rest = case rest of
  Nil -> Leaf first Nil
  _ ->
    if newestFirst then Leaf first rest
    else case reverse (Cons first rest) of
      Cons newest older -> Leaf newest older
      Nil -> Empty
