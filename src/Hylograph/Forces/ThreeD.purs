-- | d3-force-3d's forces at three dimensions, in PureScript, beside the
-- | two-dimensional kernel of `Hylograph.Forces` and with the same shape: one
-- | `Force` type, `applyForces`, and `tick` (cool, apply, move).
-- |
-- | Held to d3-force-3d 3.0.6 at `numDimensions(3)` bit for bit by
-- | `test/differential-3d.mjs`.
-- |
-- | `tick`, `applyForces` and the order of a tick are ported from d3-force-3d
-- | 3.0.6, `simulation.js` (MIT, Copyright 2017 Vasco Asturiano; itself from
-- | d3-force, ISC, Mike Bostock); see LICENSE.
module Hylograph.Forces.ThreeD
  ( Force
  , link
  , linkById
  , center
  , positionX
  , positionY
  , positionZ
  , radial
  , applyForce
  , applyForces
  , Settings
  , defaultSettings
  , tick
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect, foreachE)
import Hylograph.Forces.Random as Random
import Hylograph.Forces.ThreeD.Center (Center)
import Hylograph.Forces.ThreeD.Center as Center
import Hylograph.Forces.ThreeD.Link (Link)
import Hylograph.Forces.ThreeD.Link as Link
import Hylograph.Forces.ThreeD.Node (Node, Pinnable)
import Hylograph.Forces.ThreeD.Position (Position)
import Hylograph.Forces.ThreeD.Position as Position
import Hylograph.Forces.ThreeD.Radial (Radial)
import Hylograph.Forces.ThreeD.Radial as Radial
import Hylograph.Forces.ThreeD.Tick as Tick3
import Hylograph.Forces.Tick as Tick

-- | An initialized force of any kind.
data Force row
  = LinkForce (Link row)
  | CenterForce (Center row)
  | PositionXForce (Position row)
  | PositionYForce (Position row)
  | PositionZForce (Position row)
  | RadialForce (Radial row)

link
  :: forall row linkRow
   . Link.Config (Link.Endpoints linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Link.Endpoints linkRow)
  -> Force row
link config random nodes links = LinkForce (Link.initialize config random nodes links)

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

center :: forall row. Center.Config -> Array (Node row) -> Force row
center config nodes = CenterForce (Center.initialize config nodes)

positionX :: forall row. Position.Config row -> Array (Node row) -> Force row
positionX config nodes = PositionXForce (Position.initialize config nodes)

positionY :: forall row. Position.Config row -> Array (Node row) -> Force row
positionY config nodes = PositionYForce (Position.initialize config nodes)

positionZ :: forall row. Position.Config row -> Array (Node row) -> Force row
positionZ config nodes = PositionZForce (Position.initialize config nodes)

radial :: forall row. Radial.Config row -> Array (Node row) -> Force row
radial config nodes = RadialForce (Radial.initialize config nodes)

-- | Apply one force at the given alpha: d3's `force(alpha)`.
applyForce :: forall row. Number -> Force row -> Effect Unit
applyForce alpha force = case force of
  LinkForce state -> Link.apply state alpha
  CenterForce state -> Center.apply state
  PositionXForce state -> Position.applyX state alpha
  PositionYForce state -> Position.applyY state alpha
  PositionZForce state -> Position.applyZ state alpha
  RadialForce state -> Radial.apply state alpha

-- | Apply forces in order, each seeing what the ones before it did.
applyForces :: forall row. Number -> Array (Force row) -> Effect Unit
applyForces alpha forces = foreachE forces (applyForce alpha)

-- | The settings of a tick that are not forces.
type Settings = { alphaTarget :: Number, alphaDecay :: Number, velocityRetention :: Number }

-- | d3's defaults.
defaultSettings :: Settings
defaultSettings =
  { alphaTarget: 0.0
  , alphaDecay: Tick.defaultAlphaDecay
  , velocityRetention: Tick.defaultVelocityRetention
  }

-- | One tick of d3-force-3d's simulation: cool alpha, apply the forces at
-- | the cooled alpha, move the nodes. Returns the cooled alpha, to pass to
-- | the next tick.
tick :: forall row. Settings -> Array (Force (Pinnable row)) -> Array (Node (Pinnable row)) -> Number -> Effect Number
tick settings forces nodes alpha = do
  let cooled = Tick.cool { alphaTarget: settings.alphaTarget, alphaDecay: settings.alphaDecay } alpha
  applyForces cooled forces
  Tick3.integrate settings.velocityRetention nodes
  pure cooled
