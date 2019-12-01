import System.IO

readInput :: String -> IO [Int]
readInput path = do
  contents <- readFile path
  return $ read <$> lines contents

massToFuel :: Int -> Int
massToFuel = (max 0) . (flip (-) 2) . floor . (flip (/) 3) . fromIntegral

moduleMassToFuel :: Int -> Int
moduleMassToFuel 0 = 0
moduleMassToFuel m = fuel + moduleMassToFuel fuel
  where fuel = massToFuel m

part1 :: [Int] -> Int
part1 massValues = 
  sum $ massToFuel <$> massValues

part2 :: [Int] -> Int
part2 massValues =
  sum $ moduleMassToFuel <$> massValues

main :: IO ()
main = do
  massValues <- readInput "input"
  putStrLn $ show (part1 massValues, part2 massValues)