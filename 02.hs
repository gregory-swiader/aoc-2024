readInput :: String -> [[Int]]
readInput s = map (map read . words) $ lines s

computeA :: [[Int]] -> Int
computeA = length . filter id . map safe

computeB :: [[Int]] -> Int
computeB = length . filter id . map (any safe . genAll)
  where
    genAll = foldr (\e acc -> last acc : map (e :) acc) [[]]

safe :: [Int] -> Bool
safe l = all asc diffs || all desc diffs
  where
    asc e = 1 <= e && e <= 3
    desc e = -3 <= e && e <= -1
    diffs = zipWith (-) (init l) (tail l)

main :: IO ()
main = do
  str <- getContents
  let l = readInput str
  print $ computeA l
  print $ computeB l
