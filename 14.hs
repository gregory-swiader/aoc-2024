import Control.Arrow (first, second)
import Data.List (partition)
import Data.List.Split (splitOn)

data Robot = Robot
  { startx :: Int,
    starty :: Int,
    velx :: Int,
    vely :: Int
  }
  deriving (Show)

readInput :: String -> [Robot]
readInput = map (mkRobot . concatMap extractNumbers . splitOn " ") . lines
  where
    mkRobot [x, y, vx, vy] = Robot x y vx vy
    extractNumbers = map read . splitOn "," . last . splitOn "="

modAdd :: Int -> Int -> Int -> Int
modAdd a b m = ((a + b) `mod` m + m) `mod` m

computeA :: [Robot] -> Int
computeA r = pp * pn * nn * np
  where
    pp = length $ filter (\(x, y) -> 0 < x && 0 < y) finalLocations
    pn = length $ filter (\(x, y) -> 0 < x && 0 > y) finalLocations
    nn = length $ filter (\(x, y) -> 0 > x && 0 > y) finalLocations
    np = length $ filter (\(x, y) -> 0 > x && 0 < y) finalLocations
    finalLocations = map (shift . move) r
    shift (x, y) = (x - modx `div` 2, y - mody `div` 2)
    move (Robot x y vx vy) =
      ( modAdd x (time * vx) modx,
        modAdd y (time * vy) mody
      )
    modx = 101
    mody = 103
    time = 100

-- there's no way I'm doing this iterative simulation in haskell
computeB :: [Robot] -> Int
computeB _ = 0

main :: IO ()
main = do
  s <- getContents
  let r = readInput s
  print $ computeA r
  print $ computeB r
