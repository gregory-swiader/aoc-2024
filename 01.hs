import Data.List (sort)

readInput :: String -> ([Int], [Int])
readInput s = (sort . parse head $ s, sort . parse last $ s)
  where
    parse f = map (read . f . words) . lines

computeA :: [Int] -> [Int] -> Int
computeA l r = foldl (\acc e -> acc + abs (uncurry (-) e)) 0 $ zip l r

computeB :: [Int] -> [Int] -> Int
computeB l r = sum $ map (\el -> el * length (filter (== el) r)) l

main :: IO ()
main = do
  str <- getContents
  let (l, r) = readInput str
  print $ computeA l r
  print $ computeB l r
