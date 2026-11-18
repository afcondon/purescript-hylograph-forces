-- | Links joined to their nodes: what the link force and the directional
-- | constraint both start from.
-- |
-- | The degree counts and the bias are d3-force 3.0.0's (`link.js`,
-- | `initialize`; ISC, Copyright 2010-2021 Mike Bostock; see LICENSE).
-- |
-- | A link can name its nodes by their place in the node array (`joinByPlace`) or
-- | by an id read off each node (`joinById`). By place, a link that names a
-- | place outside the array is dropped. By id, a link that names an id no
-- | node has is an error, with d3's message.
module Hylograph.Forces.Pure.Linkage
  ( Endpoints
  , Identified
  , Joined
  , joinByPlace
  , joinById
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array (catMaybes, index, length, mapWithIndex, replicate)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Hylograph.Forces.Pure.Node (Node)

-- | A link, by the places of its two nodes in the node array.
type Endpoints row = { source :: Int, target :: Int | row }

-- | A link, by the ids of its two nodes.
type Identified id row = { source :: id, target :: id | row }

-- | A link with its nodes found. The degrees are d3's `count`: how many
-- | links touch each end. `bias` is d3's
-- | `count[source] / (count[source] + count[target])`: the share of a
-- | correction that the target takes, so that the better connected end moves
-- | less.
type Joined row link =
  { source :: Node row
  , target :: Node row
  , sourceDegree :: Number
  , targetDegree :: Number
  , bias :: Number
  , link :: link
  }

-- | Join links that name their nodes by place. A link naming a place
-- | outside the array is dropped.
joinByPlace :: forall row linkRow. Array (Node row) -> Array (Endpoints linkRow) -> Array (Joined row (Endpoints linkRow))
joinByPlace nodes links = joinPlaces nodes (map (\link -> { source: link.source, target: link.target, link }) links)

-- | Join links that name their nodes by id: d3's `force.id(...)`. Where two
-- | nodes share an id the later one is meant, as in d3. A link whose source
-- | or target is no node's id gives d3's error, `node not found: <id>`.
joinById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Array (Node row)
  -> Array (Identified id linkRow)
  -> Either String (Array (Joined row (Identified id linkRow)))
joinById identify nodes links = map (joinPlaces nodes) (traverse placed links)
  where
  places = Map.fromFoldable (mapWithIndex (\place node -> Tuple (identify node) place) nodes)

  find id = case Map.lookup id places of
    Just place -> Right place
    Nothing -> Left ("node not found: " <> show id)

  placed link = case find link.source of
    Left problem -> Left problem
    Right source -> case find link.target of
      Left problem -> Left problem
      Right target -> Right { source, target, link }

joinPlaces
  :: forall row link
   . Array (Node row)
  -> Array { source :: Int, target :: Int, link :: link }
  -> Array (Joined row link)
joinPlaces nodes links = catMaybes (map joined links)
  where
  count = length nodes

  connected link = link.source >= 0 && link.source < count && link.target >= 0 && link.target < count

  -- d3's `count`: how many links touch each node.
  degrees :: Array Int
  degrees = ST.run do
    counts <- STArray.thaw (replicate count 0)
    ST.foreach links \link ->
      when (connected link) do
        _ <- STArray.modify link.source (_ + 1) counts
        _ <- STArray.modify link.target (_ + 1) counts
        pure unit
    STArray.unsafeFreeze counts

  degree place = toNumber (fromMaybe 0 (index degrees place))

  joined link = case index nodes link.source, index nodes link.target of
    Just source, Just target -> Just
      { source
      , target
      , sourceDegree: degree link.source
      , targetDegree: degree link.target
      , bias: degree link.source / (degree link.source + degree link.target)
      , link: link.link
      }
    _, _ -> Nothing
