module Metrics
  ( manhattanDistance
  , summaryDelayDistance
  )
where

import           Wire                           ( Point(..)
                                                , WireSegment(..)
                                                , Wire(..)
                                                , getSegments
                                                )


manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = (abs x) + (abs y)

distInsideSegment :: Point -> WireSegment -> Int
distInsideSegment p (WireSegment (a, _)) = verticalDist + horizontalDist
 where
  verticalDist   = abs (y p - (y a))
  horizontalDist = abs (x p - (x a))

segmentLen :: WireSegment -> Int
segmentLen (WireSegment (Point ax ay, Point bx by)) =
  (abs (ax - bx)) + (abs (ay - by))

containedIn :: Point -> WireSegment -> Bool
containedIn (Point x y) (WireSegment (Point ax ay, Point bx by)) =
  (x >= min ax bx) && (x <= max ax bx) && (y >= min ay by) && (y <= max ay by)

delayDistance :: [WireSegment] -> Point -> Int
delayDistance (s : ss) p | containedIn p s = distInsideSegment p s
                         | otherwise       = segmentLen s + delayDistance ss p

summaryDelayDistance :: (Wire, Wire) -> Point -> Int
summaryDelayDistance (wire1, wire2) p =
  delayDistance (getSegments wire1) p + delayDistance (getSegments wire2) p
