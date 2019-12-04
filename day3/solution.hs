module Main where

import           Wire                           ( Wire
                                                , closestIntersection
                                                , contentToWire
                                                )
import           Metrics                        ( manhattanDistance
                                                , summaryDelayDistance
                                                )

loadInput :: String -> IO (Maybe (Wire, Wire))
loadInput path = do
  content <- readFile path
  return $ contentToWire content

part1 :: (Wire, Wire) -> Int
part1 wires = closestIntersection manhattanDistance wires

part2 :: (Wire, Wire) -> Int
part2 wires = closestIntersection (summaryDelayDistance wires) wires

main :: IO ()
main = do
  input <- loadInput "inputs/input"
  putStrLn $ show $ (\wires -> (part1 wires, part2 wires)) <$> input
