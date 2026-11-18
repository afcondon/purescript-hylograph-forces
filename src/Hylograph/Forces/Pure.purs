-- | d3-force's forces in PureScript, with no JavaScript for the algorithms,
-- | and the six things the Rust kernel has that d3 does not.
-- |
-- | A spike. Each force has its own module with an `initialize` and an
-- | `apply`; this module gathers them into one `Force` type and gives the
-- | loop that `Hylograph.ForceEngine.Core` runs today around d3's functions:
-- | cool, apply each force, move the nodes, and then (the Rust kernel's
-- | addition) apply any hard containment and put pinned nodes back.
-- |
-- | The d3 forces are held to d3-force 3.0.0 bit for bit, and the others to
-- | the Rust kernel built at 64 bits, by `test/differential.mjs`.
module Hylograph.Forces.Pure
  ( Force
  , manyBody
  , link
  , linkById
  , collide
  , center
  , positionX
  , positionY
  , radial
  , towards
  , rectCollide
  , contain
  , cohesion
  , directional
  , directionalById
  , applyForce
  , applyForces
  , constrain
  , Settings
  , defaultSettings
  , tick
  ) where

import Prelude

import Data.Array (any)
import Data.Either (Either)
import Effect (Effect, foreachE)
import Hylograph.Forces.Pure.Center (Center)
import Hylograph.Forces.Pure.Center as Center
import Hylograph.Forces.Pure.Collide (Collide)
import Hylograph.Forces.Pure.Cohesion (Cohesion)
import Hylograph.Forces.Pure.Cohesion as Cohesion
import Hylograph.Forces.Pure.Collide as Collide
import Hylograph.Forces.Pure.Contain (Contain)
import Hylograph.Forces.Pure.Contain as Contain
import Hylograph.Forces.Pure.Directional (Directional)
import Hylograph.Forces.Pure.Directional as Directional
import Hylograph.Forces.Pure.Link (Link)
import Hylograph.Forces.Pure.Link as Link
import Hylograph.Forces.Pure.ManyBody (ManyBody)
import Hylograph.Forces.Pure.ManyBody as ManyBody
import Hylograph.Forces.Pure.Node (Node, Pinnable)
import Hylograph.Forces.Pure.Position (Position)
import Hylograph.Forces.Pure.Position as Position
import Hylograph.Forces.Pure.Radial (Radial)
import Hylograph.Forces.Pure.Radial as Radial
import Hylograph.Forces.Pure.Random as Random
import Hylograph.Forces.Pure.RectCollide (RectCollide)
import Hylograph.Forces.Pure.RectCollide as RectCollide
import Hylograph.Forces.Pure.Tick as Tick
import Hylograph.Forces.Pure.Towards (Towards)
import Hylograph.Forces.Pure.Towards as Towards

-- | An initialized force of any kind.
data Force row
  = ManyBodyForce (ManyBody row)
  | LinkForce (Link row)
  | CollideForce (Collide row)
  | CenterForce (Center row)
  | PositionXForce (Position row)
  | PositionYForce (Position row)
  | RadialForce (Radial row)
  | TowardsForce (Towards row)
  | RectCollideForce (RectCollide row)
  | ContainForce (Contain row)
  | CohesionForce (Cohesion row)
  | DirectionalForce (Directional row)

manyBody :: forall row. ManyBody.Config row -> Random.Source -> Array (Node row) -> Force row
manyBody config random nodes = ManyBodyForce (ManyBody.initialize config random nodes)

link
  :: forall row linkRow
   . Link.Config (Link.Endpoints linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Link.Endpoints linkRow)
  -> Force row
link config random nodes links = LinkForce (Link.initialize config random nodes links)

collide :: forall row. Collide.Config row -> Random.Source -> Array (Node row) -> Force row
collide config random nodes = CollideForce (Collide.initialize config random nodes)

center :: forall row. Center.Config -> Array (Node row) -> Force row
center config nodes = CenterForce (Center.initialize config nodes)

positionX :: forall row. Position.Config row -> Array (Node row) -> Force row
positionX config nodes = PositionXForce (Position.initialize config nodes)

positionY :: forall row. Position.Config row -> Array (Node row) -> Force row
positionY config nodes = PositionYForce (Position.initialize config nodes)

radial :: forall row. Radial.Config row -> Array (Node row) -> Force row
radial config nodes = RadialForce (Radial.initialize config nodes)

-- | The link force for links that name their nodes by id; d3's error where
-- | a link names an id no node has.
linkById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Link.Config (Link.Identified id linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Link.Identified id linkRow)
  -> Either String (Force row)
linkById identify config random nodes links = map LinkForce (Link.initializeById identify config random nodes links)

towards :: forall row. Towards.Config row -> Array (Node row) -> Force row
towards config nodes = TowardsForce (Towards.initialize config nodes)

rectCollide :: forall row. RectCollide.Config row -> Array (Node row) -> Force row
rectCollide config nodes = RectCollideForce (RectCollide.initialize config nodes)

contain :: forall row. Contain.Config row -> Array (Node row) -> Force row
contain config nodes = ContainForce (Contain.initialize config nodes)

cohesion :: forall row. Cohesion.Config row -> Array (Node row) -> Force row
cohesion config nodes = CohesionForce (Cohesion.initialize config nodes)

directional
  :: forall row linkRow
   . Directional.Config (Link.Endpoints linkRow)
  -> Array (Node row)
  -> Array (Link.Endpoints linkRow)
  -> Force row
directional config nodes links = DirectionalForce (Directional.initialize config nodes links)

directionalById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Directional.Config (Link.Identified id linkRow)
  -> Array (Node row)
  -> Array (Link.Identified id linkRow)
  -> Either String (Force row)
directionalById identify config nodes links = map DirectionalForce (Directional.initializeById identify config nodes links)

-- | Apply one force at the given alpha: d3's `force(alpha)`.
applyForce :: forall row. Number -> Force row -> Effect Unit
applyForce alpha force = case force of
  ManyBodyForce state -> ManyBody.apply state alpha
  LinkForce state -> Link.apply state alpha
  CollideForce state -> Collide.apply state alpha
  CenterForce state -> Center.apply state
  PositionXForce state -> Position.applyX state alpha
  PositionYForce state -> Position.applyY state alpha
  RadialForce state -> Radial.apply state alpha
  TowardsForce state -> Towards.apply state alpha
  RectCollideForce state -> RectCollide.apply state
  ContainForce state -> Contain.apply state
  CohesionForce state -> Cohesion.apply state alpha
  DirectionalForce state -> Directional.apply state alpha

-- | Apply forces in order, each seeing what the ones before it did.
applyForces :: forall row. Number -> Array (Force row) -> Effect Unit
applyForces alpha forces = foreachE forces (applyForce alpha)

-- | The part of the forces that runs after the nodes have moved: every hard
-- | containment clamps its nodes into their regions, in the order the forces
-- | are listed, and then, if anything clamped, pinned coordinates are put
-- | back, so that a pin beats a region. Nothing at all where no force is a
-- | hard containment.
constrain :: forall row. Array (Force (Pinnable row)) -> Array (Node (Pinnable row)) -> Effect Unit
constrain forces nodes =
  when (any clamps forces) do
    foreachE forces \force -> case force of
      ContainForce state -> Contain.constrain state
      _ -> pure unit
    Tick.pin nodes
  where
  clamps force = case force of
    ContainForce state -> Contain.isHard state
    _ -> false

-- | The settings of a tick that are not forces.
type Settings = { alphaTarget :: Number, alphaDecay :: Number, velocityRetention :: Number }

-- | d3's defaults.
defaultSettings :: Settings
defaultSettings =
  { alphaTarget: 0.0
  , alphaDecay: Tick.defaultAlphaDecay
  , velocityRetention: Tick.defaultVelocityRetention
  }

-- | One tick of d3's simulation: cool alpha, apply the forces at the cooled
-- | alpha, move the nodes; and then the Rust kernel's last step, `constrain`.
-- | Returns the cooled alpha, to pass to the next tick.
tick :: forall row. Settings -> Array (Force (Pinnable row)) -> Array (Node (Pinnable row)) -> Number -> Effect Number
tick settings forces nodes alpha = do
  let cooled = Tick.cool { alphaTarget: settings.alphaTarget, alphaDecay: settings.alphaDecay } alpha
  applyForces cooled forces
  Tick.integrate settings.velocityRetention nodes
  constrain forces nodes
  pure cooled
