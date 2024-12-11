import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet

readInput :: String -> MultiSet Int
readInput = MultiSet.fromList . map read . words

update :: Int -> [Int]
update n
  | n == 0 = [1]
  | odd . length $ s = [2024 * n]
  | even . length $ s = [read . take l $ s, read . drop l $ s]
  where
    s = show n
    l = length s `div` 2

blink :: MultiSet Int -> MultiSet Int
blink = MultiSet.concatMap update

computeA :: MultiSet Int -> Int
computeA = MultiSet.size . (!! 25) . iterate blink

computeB :: MultiSet Int -> Int
computeB = MultiSet.size . (!! 75) . iterate blink

main = do
  s <- getContents
  let l = readInput s
  print $ computeA l
  print $ computeB l
