import Control.Arrow (second, (&&&))

readInput :: String -> [(Int, [Int])]
-- readInput = map ((\l -> ((read . filter (/= ':') . head) l, (map read . tail) l)) . words) . lines
readInput = map (((read . filter (/= ':') . head) &&& (map read . tail)) . words) . lines

compute :: (Int -> Int -> [Int]) -> [(Int, [Int])] -> Int
-- compute f ll = sum . map fst $ filter (uncurry elem) $ map (\(t, l) -> (t, res l)) ll
compute f ll = sum . map fst $ filter (uncurry elem) $ map (second res) ll
  where
    res l = foldl (\acc e -> concatMap (`f` e) acc) [head l] (tail l)

computeA :: [(Int, [Int])] -> Int
computeA = compute (\a e -> [a + e, a * e])

computeB :: [(Int, [Int])] -> Int
computeB = compute (\a e -> [a + e, a * e, read (show a ++ show e)])

main :: IO ()
main = do
  s <- getContents
  let l = readInput s
  print $ computeA l
  print $ computeB l
