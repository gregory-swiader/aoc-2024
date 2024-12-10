{-# LANGUAGE LambdaCase #-}

import Control.Arrow (first, second)
import Data.Char (digitToInt)
import Data.List.Split (splitOn, splitWhen)

data Block = File {run :: Int, len :: Int} | Space {len :: Int}
  deriving (Eq, Show)

readInput :: String -> ([Block], [Block])
readInput s = (files, spaces)
  where
    l = zip [0 ..] . map digitToInt . head . lines $ s
    files = zipWith File [0 ..] . map snd . filter (even . fst) $ l
    spaces = map (Space . snd) . filter (odd . fst) $ l

checksum :: [Block] -> Int
checksum = snd . foldl (\(i, s) f -> (i + len f, s + csumFile i f)) (0, 0)
  where
    csumFile i = \case File r l -> r * arithmSum i 1 l; Space _ -> 0
    arithmSum a r n = n * a + (r * (n - 1) * n) `div` 2

fillSpace :: Block -> [Block] -> ([Block], [Block])
fillSpace _ [] = ([], [])
fillSpace space files =
  let file = last files
      movedFile = File (run file) (min (len space) (len file))
      leftFile = File (run file) (len file - len space)
      leftSpace = Space (len space - len file)
   in case compare (len space) (len file) of
        EQ -> ([movedFile], init files)
        LT -> ([movedFile], init files ++ [leftFile])
        GT -> first (movedFile :) $ fillSpace leftSpace (init files)

computeA :: [Block] -> [Block] -> Int
computeA files spaces = checksum $ compact files spaces
  where
    compact :: [Block] -> [Block] -> [Block]
    compact [] _ = []
    compact (f : files) (s : spaces) =
      f : uncurry (++) (second (`compact` spaces) $ fillSpace s files)

merge :: [a] -> [a] -> [a]
merge [] r = r
merge l [] = l
merge (l : ls) (r : rs) = l : r : merge ls rs

computeB :: [Block] -> [Block] -> Int
computeB files spaces = checksum $ foldr moveFile (merge files spaces) files
  where
    moveFile file disk =
      let [left, right] = splitOn [file] disk
          findSpace = \case Space l -> l >= len file; File _ _ -> False
       in case break findSpace left of
            (_, []) -> disk
            (offset, space : rest) ->
              let movedFile = File (run file) (min (len space) (len file))
                  leftSpace = Space (len space - len file)
                  newSpace = Space (len file)
               in offset ++ [movedFile, leftSpace] ++ rest ++ newSpace : right

main :: IO ()
main = do
  s <- getContents
  let (files, spaces) = readInput s
  print $ computeA files spaces
  print $ computeB files spaces
