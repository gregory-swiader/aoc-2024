import Data.List (delete)
import Data.List.Split (splitOn)

readInput :: String -> ([[Int]], [[Int]])
readInput s = (splitBy "|" rules, splitBy "," lists)
  where
    [rules, lists] = map lines . splitOn "\n\n" $ s
    splitBy c = map (map read . splitOn c)

compute :: ([Int] -> [Int]) -> (Bool -> Bool) -> [[Int]] -> [[Int]] -> Int
compute transform view rules = sum . map (middle . transform) . filter (view . isTopoSortable)
  where
    middle l = l !! (length l `div` 2)
    isTopoSortable :: [Int] -> Bool
    isTopoSortable [] = True
    isTopoSortable (x : xs) = (not . any ((`elem` rules) . (: [x]))) xs && isTopoSortable xs

computeA :: [[Int]] -> [[Int]] -> Int
computeA = compute id id

computeB :: [[Int]] -> [[Int]] -> Int
computeB rules = compute makeSortable not rules
  where
    makeSortable :: [Int] -> [Int]
    makeSortable [] = []
    makeSortable l = good : makeSortable (delete good l)
      where
        good = findGood [] l
        findGood :: [Int] -> [Int] -> Int
        findGood r (x : xs)
          | (not . any ((`elem` rules) . (: [x]))) (r ++ xs) = x
          | otherwise = findGood (x : r) xs

main :: IO ()
main = do
  s <- getContents
  let (rules, lists) = readInput s
  print $ computeA rules lists
  print $ computeB rules lists
