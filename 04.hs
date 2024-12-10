import Data.List (transpose)
import Data.List.Split (chunksOf, divvy, splitOn)

count :: String -> String -> Int
count needle haystack = length (splitOn needle haystack) - 1

readInput :: String -> [String]
readInput = lines

computeA :: [String] -> Int
computeA rows = sum . map (count "XMAS") $ allDirections
  where
    allDirections = forthAndBack (rows ++ columns ++ positiveDiagonals ++ negativeDiagonals)
    forthAndBack = map (\t -> t ++ reverse t)
    columns = transpose rows
    positiveDiagonals = negDiagonals . map reverse $ rows
    negativeDiagonals = negDiagonals rows
    negDiagonals r = foldr f (chunksOf 1 (last r)) (init r)
      where
        f (c : cs) acc = [c] : zipWith (:) cs (init acc) ++ drop (length cs) acc

computeB :: [String] -> Int
computeB = length . filter isXMAS . windows
  where
    windows = concatMap (transpose . map (divvy 3 1)) . divvy 3 1
    isXMAS = (`elem` patterns) . inspect . window
    inspect = map snd . filter (even . fst) . zip [0 ..]
    window = concatMap (take 3) . take 3
    patterns =
      map
        (inspect . window)
        [ [ "M.M",
            ".A.",
            "S.S"
          ],
          [ "M.S",
            ".A.",
            "M.S"
          ],
          [ "S.S",
            ".A.",
            "M.M"
          ],
          [ "S.M",
            ".A.",
            "S.M"
          ]
        ]

main :: IO ()
main = do
  s <- getContents
  let l = readInput s
  print $ computeA l
  print $ computeB l
