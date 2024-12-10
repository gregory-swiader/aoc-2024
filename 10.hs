import Data.Char (digitToInt)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set

data Point = Point
  { row :: Int,
    col :: Int,
    height :: Int
  }
  deriving (Show, Eq, Ord)

neighbors :: Point -> Set Point -> Set Point
neighbors p =
  Set.filter
    ( \q ->
        case (row q - row p, col q - col p, height q - height p) of
          (0, d, 1) | d `elem` [1, -1] -> True
          (d, 0, 1) | d `elem` [1, -1] -> True
          _ -> False
    )

readInput :: String -> Set Point
readInput = Set.fromList . concat . zipWithPoint . map (map digitToInt) . lines
  where
    zipWithPoint = zipWith (\i row -> zipWith (Point i) [0 ..] row) [0 ..]

dfs :: Point -> Set Point -> MultiSet Point
dfs p@(Point _ _ 9) _ = MultiSet.singleton p
dfs start world = foldl iteration MultiSet.empty (neighbors start world)
  where
    iteration acc n = acc `MultiSet.union` dfs n world

computeA :: [MultiSet Point] -> Int
computeA = sum . map score
  where
    score = MultiSet.distinctSize

computeB :: [MultiSet Point] -> Int
computeB = sum . map rate
  where
    rate = MultiSet.size

runDFS :: Set Point -> [MultiSet Point]
runDFS w = map (`dfs` w) trailheads
  where
    trailheads = Set.toList $ Set.filter ((== 0) . height) w

main :: IO ()
main = do
  s <- getContents
  let w = readInput s
      m = runDFS w
  print $ computeA m
  print $ computeB m
