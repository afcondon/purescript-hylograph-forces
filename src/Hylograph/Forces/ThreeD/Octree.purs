-- | The part of d3-octree that d3-force-3d uses: build a tree from a set of
-- | points, once, and hand it to a force to walk.
-- |
-- | Ported from d3-octree 1.1.0 (MIT, Copyright 2017 Vasco Asturiano; itself
-- | from d3-quadtree, ISC, Mike Bostock); see LICENSE.
-- |
-- | Built the way `Hylograph.Forces.Quadtree` builds its tree, from the top
-- | down and without mutation, and for the same reason: d3-octree splits a
-- | cube exactly when a second location lands in it, so the tree does not
-- | depend on the order of insertion. What does depend on it is kept: a
-- | leaf's points run newest first, and the extent starts from the unit cube
-- | at the floor of the smallest coordinates and doubles until it covers the
-- | largest. d3-octree's `cover` measures the side by x alone
-- | (`t = x1 - x0 || 1`) and doubles all three axes together, so the extent
-- | is always a cube.
-- |
-- | The eight octants of a branch are in d3-octree's order, the index being
-- | `deep << 2 | bottom << 1 | right`, where right is `x >= middle`, bottom
-- | `y >= middle` and deep `z >= middle`. Its `visit` takes them in that
-- | order.
-- |
-- | One thing d3-quadtree does not do. `addAll` keeps the coordinates in
-- | `Float64Array`s, and a point with a `NaN` coordinate is skipped when they
-- | are filled, so its slots stay `0`: it is left out of the extent, and then
-- | inserted all the same, at (0, 0, 0). (d3-quadtree's plain arrays leave
-- | the slot `undefined`, and `add` drops the point.) Such a point descends
-- | the tree as if it were at the origin, but where it is the leaf that a
-- | later point is compared with or split from, its own coordinates are read
-- | again (`xp = +tree._x.call(null, node.data)`), and they are `NaN`. Where
-- | it lands then depends on the order of insertion, so when any point has a
-- | `NaN` coordinate this module builds the tree as d3-octree does, one point
-- | at a time (`insert`).
module Hylograph.Forces.ThreeD.Octree
  ( Point
  , Octree(..)
  , Extent
  , Planted
  , build
  ) where

import Prelude

import Data.Function.Uncurried (Fn8, mkFn8, runFn8)
import Data.List (List(..), reverse)
import Data.Number (floor, infinity, isNaN)

-- | A location and whatever the force keeps there.
type Point item = { x :: Number, y :: Number, z :: Number, item :: item }

-- | An octree. A `Leaf` holds its newest point and then the older points at
-- | the same location, newest first. A `Branch` holds its eight octants in
-- | d3-octree's order (see the module's head).
data Octree item
  = Empty
  | Leaf (Point item) (List (Point item))
  | Branch (Octree item) (Octree item) (Octree item) (Octree item) (Octree item) (Octree item) (Octree item) (Octree item)

-- | The cube a tree covers.
type Extent = { x0 :: Number, y0 :: Number, z0 :: Number, x1 :: Number, y1 :: Number, z1 :: Number }

-- | A tree and the cube its root covers.
type Planted item = { extent :: Extent, root :: Octree item }

-- | d3-octree's `octree(nodes, x, y, z)`: the tree of the given points,
-- | listed newest first (the reverse of d3's insertion order). A point with
-- | any coordinate `NaN` is left out, as d3-octree leaves it out.
build :: forall item. List (Point item) -> Planted item
build newestFirst = survey newestFirst Nil false infinity infinity infinity (-infinity) (-infinity) (-infinity)
  where
  -- `addAll`, first half: the extent of the valid points. The points that
  -- are kept come out oldest first.
  survey remaining kept spoilt x0 y0 z0 x1 y1 z1 = case remaining of
    Nil ->
      if x0 > x1 || y0 > y1 || z0 > z1 then { extent: { x0, y0, z0, x1, y1, z1 }, root: Empty }
      else
        let
          extent = cover (unitCube x0 y0 z0) x1 y1 z1
        in
          { extent
          , root:
              if spoilt then insertAll extent (reverse newestFirst) Empty
              else runFn8 cell false extent.x0 extent.y0 extent.z0 extent.x1 extent.y1 extent.z1 kept
          }
    Cons point rest ->
      if isNaN point.x || isNaN point.y || isNaN point.z then survey rest kept true x0 y0 z0 x1 y1 z1
      else survey rest (Cons point kept) spoilt
        (if point.x < x0 then point.x else x0)
        (if point.y < y0 then point.y else y0)
        (if point.z < z0 then point.z else z0)
        (if point.x > x1 then point.x else x1)
        (if point.y > y1 then point.y else y1)
        (if point.z > z1 then point.z else z1)

-- | `cover` on a tree with no extent yet: the unit cube at the floor of the
-- | point.
unitCube :: Number -> Number -> Number -> Extent
unitCube x y z =
  let
    x0 = floor x
    y0 = floor y
    z0 = floor z
  in
    { x0, y0, z0, x1: x0 + 1.0, y1: y0 + 1.0, z1: z0 + 1.0 }

-- | `cover` on a tree that has an extent: double the cube, towards the
-- | point on each axis, until the point is inside.
-- |
-- | ```
-- | var t = x1 - x0 || 1;
-- | while (x0 > x || x >= x1 || y0 > y || y >= y1 || z0 > z || z >= z1) {
-- |   i = (z < z0) << 2 | (y < y0) << 1 | (x < x0);
-- |   t *= 2;
-- |   // for each axis: if its bit is set, low = high - t, else high = low + t
-- | }
-- | ```
cover :: Extent -> Number -> Number -> Number -> Extent
cover extent x y z = grow extent.x0 extent.y0 extent.z0 extent.x1 extent.y1 extent.z1 side
  where
  width = extent.x1 - extent.x0
  side = if width < 0.0 || width > 0.0 then width else 1.0

  grow x0 y0 z0 x1 y1 z1 size =
    if x0 > x || x >= x1 || y0 > y || y >= y1 || z0 > z || z >= z1 then
      let
        doubled = size * 2.0
        towardsX = x < x0
        towardsY = y < y0
        towardsZ = z < z0
      in
        grow
          (if towardsX then x1 - doubled else x0)
          (if towardsY then y1 - doubled else y0)
          (if towardsZ then z1 - doubled else z0)
          (if towardsX then x1 else x0 + doubled)
          (if towardsY then y1 else y0 + doubled)
          (if towardsZ then z1 else z0 + doubled)
          doubled
    else { x0, y0, z0, x1, y1, z1 }

-- | The tree of the points that fall in one cube: whether its list runs
-- | newest first, its corners, and the list. Dealing points into octants
-- | reverses them, so the direction alternates with depth and a leaf may
-- | have to turn its list round.
cell :: forall item. Fn8 Boolean Number Number Number Number Number Number (List (Point item)) (Octree item)
cell = mkFn8 \newestFirst x0 y0 z0 x1 y1 z1 points ->
  let
    middleX = (x0 + x1) / 2.0
    middleY = (y0 + y1) / 2.0
    middleZ = (z0 + z1) / 2.0
    turned = not newestFirst

    -- The octants, named by d3-octree's index: o0 is (x < middle, y <
    -- middle, z < middle), o1 has x >= middle, o2 y >= middle, o4 z >=
    -- middle, and the others are the sums.
    deal remaining o0 o1 o2 o3 o4 o5 o6 o7 = case remaining of
      Nil ->
        Branch
          (runFn8 cell turned x0 y0 z0 middleX middleY middleZ o0)
          (runFn8 cell turned middleX y0 z0 x1 middleY middleZ o1)
          (runFn8 cell turned x0 middleY z0 middleX y1 middleZ o2)
          (runFn8 cell turned middleX middleY z0 x1 y1 middleZ o3)
          (runFn8 cell turned x0 y0 middleZ middleX middleY z1 o4)
          (runFn8 cell turned middleX y0 middleZ x1 middleY z1 o5)
          (runFn8 cell turned x0 middleY middleZ middleX y1 z1 o6)
          (runFn8 cell turned middleX middleY middleZ x1 y1 z1 o7)
      Cons point rest ->
        if point.z >= middleZ then
          if point.y >= middleY then
            if point.x >= middleX then deal rest o0 o1 o2 o3 o4 o5 o6 (Cons point o7)
            else deal rest o0 o1 o2 o3 o4 o5 (Cons point o6) o7
          else if point.x >= middleX then deal rest o0 o1 o2 o3 o4 (Cons point o5) o6 o7
          else deal rest o0 o1 o2 o3 (Cons point o4) o5 o6 o7
        else if point.y >= middleY then
          if point.x >= middleX then deal rest o0 o1 o2 (Cons point o3) o4 o5 o6 o7
          else deal rest o0 o1 (Cons point o2) o3 o4 o5 o6 o7
        else if point.x >= middleX then deal rest o0 (Cons point o1) o2 o3 o4 o5 o6 o7
        else deal rest (Cons point o0) o1 o2 o3 o4 o5 o6 o7
  in
    case points of
      Nil -> Empty
      Cons first rest ->
        if coincident first rest then leaf newestFirst first rest
        else deal points Nil Nil Nil Nil Nil Nil Nil Nil

-- | Whether every point of the list is at the first point's location:
-- | d3-octree's `x === xp && y === yp && z === zp`.
coincident :: forall item. Point item -> List (Point item) -> Boolean
coincident first others = case elsewhere first others of
  Nil -> true
  _ -> false

-- | The list from its first point that is not at the given point's location
-- | (a list rather than a `Boolean`, so that the recursion stays a loop
-- | under the optimising backend; see `Hylograph.Forces.Quadtree`).
elsewhere :: forall item. Point item -> List (Point item) -> List (Point item)
elsewhere first others = case others of
  Nil -> Nil
  Cons other rest ->
    if other.x == first.x && other.y == first.y && other.z == first.z then elsewhere first rest
    else others

-- | A leaf of coincident points, newest first.
leaf :: forall item. Boolean -> Point item -> List (Point item) -> Octree item
leaf newestFirst first rest = case rest of
  Nil -> Leaf first Nil
  _ ->
    if newestFirst then Leaf first rest
    else case reverse (Cons first rest) of
      Cons newest older -> Leaf newest older
      Nil -> Empty

-- | `addAll`, second half, as d3-octree does it: every point inserted in
-- | turn, oldest first, a point with a `NaN` coordinate at (0, 0, 0).
insertAll :: forall item. Extent -> List (Point item) -> Octree item -> Octree item
insertAll extent oldestFirst tree = case oldestFirst of
  Nil -> tree
  Cons point rest ->
    let
      valid = not (isNaN point.x || isNaN point.y || isNaN point.z)
    in
      insertAll extent rest
        ( insert
            { x: if valid then point.x else 0.0
            , y: if valid then point.y else 0.0
            , z: if valid then point.z else 0.0
            }
            point
            extent.x0
            extent.y0
            extent.z0
            extent.x1
            extent.y1
            extent.z1
            tree
        )

-- | Where a point is taken to be as it descends.
type Place = { x :: Number, y :: Number, z :: Number }

-- | d3-octree's `add(tree, x, y, z, d)`, given the place the point descends
-- | by, the point, the corners of the cube and the tree in it.
-- |
-- | ```
-- | while (node.length) { ...descend by x, y, z...; if (!(node = node[i])) return parent[i] = leaf; }
-- | xp = +tree._x.call(null, node.data); ...
-- | if (x === xp && y === yp && z === zp) return leaf.next = node, ...;
-- | do { ...split... } while ((i = deep << 2 | bottom << 1 | right) === (j = (zp >= zm) << 2 | (yp >= ym) << 1 | (xp >= xm)));
-- | return parent[j] = node, parent[i] = leaf;
-- | ```
insert :: forall item. Place -> Point item -> Number -> Number -> Number -> Number -> Number -> Number -> Octree item -> Octree item
insert place point x0 y0 z0 x1 y1 z1 tree = case tree of
  Empty -> Leaf point Nil
  Leaf existing older ->
    if place.x == existing.x && place.y == existing.y && place.z == existing.z then Leaf point (Cons existing older)
    else split place point existing tree x0 y0 z0 x1 y1 z1
  Branch _ _ _ _ _ _ _ _ ->
    let
      middleX = (x0 + x1) / 2.0
      middleY = (y0 + y1) / 2.0
      middleZ = (z0 + z1) / 2.0
      right = place.x >= middleX
      bottom = place.y >= middleY
      deep = place.z >= middleZ
      index = octantIndex right bottom deep
    in
      withOctant index tree
        ( insert place point
            (if right then middleX else x0)
            (if bottom then middleY else y0)
            (if deep then middleZ else z0)
            (if right then x1 else middleX)
            (if bottom then y1 else middleY)
            (if deep then z1 else middleZ)
            (octant index tree)
        )

-- | Split a cube until the point and the leaf already there are in
-- | different octants; the leaf is placed by its own coordinates.
split :: forall item. Place -> Point item -> Point item -> Octree item -> Number -> Number -> Number -> Number -> Number -> Number -> Octree item
split place point existing old x0 y0 z0 x1 y1 z1 =
  let
    middleX = (x0 + x1) / 2.0
    middleY = (y0 + y1) / 2.0
    middleZ = (z0 + z1) / 2.0
    right = place.x >= middleX
    bottom = place.y >= middleY
    deep = place.z >= middleZ
    index = octantIndex right bottom deep
    other = octantIndex (existing.x >= middleX) (existing.y >= middleY) (existing.z >= middleZ)
  in
    if index == other then
      withOctant index emptyBranch
        ( split place point existing old
            (if right then middleX else x0)
            (if bottom then middleY else y0)
            (if deep then middleZ else z0)
            (if right then x1 else middleX)
            (if bottom then y1 else middleY)
            (if deep then z1 else middleZ)
        )
    else withOctant index (withOctant other emptyBranch old) (Leaf point Nil)

emptyBranch :: forall item. Octree item
emptyBranch = Branch Empty Empty Empty Empty Empty Empty Empty Empty

-- | `deep << 2 | bottom << 1 | right`
octantIndex :: Boolean -> Boolean -> Boolean -> Int
octantIndex right bottom deep = (if deep then 4 else 0) + (if bottom then 2 else 0) + (if right then 1 else 0)

-- | One octant of a branch, by d3-octree's index.
octant :: forall item. Int -> Octree item -> Octree item
octant index tree = case tree of
  Branch o0 o1 o2 o3 o4 o5 o6 o7 -> case index of
    0 -> o0
    1 -> o1
    2 -> o2
    3 -> o3
    4 -> o4
    5 -> o5
    6 -> o6
    _ -> o7
  _ -> Empty

-- | A branch with one octant replaced.
withOctant :: forall item. Int -> Octree item -> Octree item -> Octree item
withOctant index tree replacement = case tree of
  Branch o0 o1 o2 o3 o4 o5 o6 o7 -> case index of
    0 -> Branch replacement o1 o2 o3 o4 o5 o6 o7
    1 -> Branch o0 replacement o2 o3 o4 o5 o6 o7
    2 -> Branch o0 o1 replacement o3 o4 o5 o6 o7
    3 -> Branch o0 o1 o2 replacement o4 o5 o6 o7
    4 -> Branch o0 o1 o2 o3 replacement o5 o6 o7
    5 -> Branch o0 o1 o2 o3 o4 replacement o6 o7
    6 -> Branch o0 o1 o2 o3 o4 o5 replacement o7
    _ -> Branch o0 o1 o2 o3 o4 o5 o6 replacement
  _ -> tree
