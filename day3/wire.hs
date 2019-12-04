module Wire
  ( Wire(..)
  , Point(..)
  , WireSegment(..)
  , getSegments
  , closestIntersection
  , contentToWire
  )
where

import           Control.Applicative
import           Data.List.Split
import           Data.Maybe

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)

data SegmentDirection = Vertical | Horizontal | Unknown deriving (Show, Eq)

newtype WireSegment = WireSegment (Point, Point)
  deriving (Show)
newtype Wire = Wire [Point]
  deriving (Show)

data StepDirection = DLeft | DUp | DDown | DRight deriving (Show, Eq)

data Step = Step { direction :: StepDirection
                 , distance  :: Int
                 } deriving (Show)


getSegments :: Wire -> [WireSegment]
getSegments (Wire ws) = WireSegment <$> splitPairs ws

splitPairs :: [a] -> [(a, a)]
splitPairs []       = []
splitPairs (x : []) = []
splitPairs (x : xs) = (x, head xs) : splitPairs xs


segmentDirection :: WireSegment -> SegmentDirection
segmentDirection (WireSegment (a, b)) | x a == x b = Vertical
                                      | y a == y b = Horizontal
                                      | otherwise  = Unknown

isParallel :: WireSegment -> WireSegment -> Bool
isParallel a b = segmentDirection a == segmentDirection b

intersectPerpendicular :: WireSegment -> WireSegment -> Maybe Point
intersectPerpendicular (WireSegment (a, b)) (WireSegment (c, d))
  | (y a >= min (y c) (y d))
    && (y a <= max (y c) (y d))
    && (x c >= min (x a) (x b) && (x c <= max (x a) (x b)))
  = Just $ Point (x c) (y a)
  | otherwise
  = Nothing

intersect :: WireSegment -> WireSegment -> Maybe Point
intersect a b
  | isParallel a b = Nothing
  | otherwise      = intersectPerpendicular a b <|> intersectPerpendicular b a


addPoint :: Point -> Point -> Point
addPoint a b = Point (x a + x b) (y a + y b)

makeWire :: [Step] -> Wire
makeWire steps = Wire $ scanAlongWire steps
  where scanAlongWire = scanl (flip (addPoint . stepToDelta)) (Point 0 0)

stepStringsToWire :: [String] -> Maybe Wire
stepStringsToWire strs = do
  steps <- sequence $ stringToStep <$> strs
  return $ makeWire steps

contentToWire :: String -> Maybe (Wire, Wire)
contentToWire content =
  let (line1 : line2 : _) = lines content
  in  do
        wire1 <- stepStringsToWire $ splitOn "," line1
        wire2 <- stepStringsToWire $ splitOn "," line2
        return $ (wire1, wire2)

getIntersections :: Wire -> Wire -> [Point]
getIntersections wire1 wire2 = catMaybes intersections
 where
  combinations  = [ (x, y) | x <- getSegments wire1, y <- getSegments wire2 ]
  intersections = (\(x, y) -> intersect x y) <$> combinations


closest :: (Point -> Int) -> [Point] -> Int
closest metric pts = minimum $ metric <$> ptsWithoutOrigin
  where ptsWithoutOrigin = filter (/= (Point 0 0)) pts

closestIntersection :: (Point -> Int) -> (Wire, Wire) -> Int
closestIntersection metric =
  (\(wire1, wire2) -> closest metric $ getIntersections wire1 wire2)

stringToStep :: String -> Maybe Step
stringToStep ('R' : dist) = Just $ Step DRight (read dist)
stringToStep ('L' : dist) = Just $ Step DLeft (read dist)
stringToStep ('U' : dist) = Just $ Step DUp (read dist)
stringToStep ('D' : dist) = Just $ Step DDown (read dist)
stringToStep _            = Nothing

stepToDelta :: Step -> Point
stepToDelta (Step direction distance) | direction == DRight = Point distance 0
                                      | direction == DLeft = Point (-distance) 0
                                      | direction == DUp = Point 0 (-distance)
                                      | direction == DDown = Point 0 distance
