{-# LANGUAGE LambdaCase #-}

import Data.Function (on)
import Data.List (groupBy, nub, sortBy, subsequences)

data Point = Cell
  { x :: Int,
    y :: Int
  }
  deriving (Eq, Show)

(<+>) :: Point -> Point -> Point
(<+>) p q = Cell (x p + x q) (y p + y q)

(<->) :: Point -> Point -> Point
(<->) p q = Cell (x p - x q) (y p - y q)

(</>) :: Point -> Int -> Point
(</>) p d = Cell (x p `div` d) (y p `div` d)

(<%>) :: Point -> Int -> Point
(<%>) p m = Cell (x p `mod` m) (y p `mod` m)

within :: Point -> Point -> Bool
within p dim = 0 <= x p && x p < x dim && 0 <= y p && y p < y dim

step :: Point -> Point -> Point
step p q = d </> gcd (x d) (y d) where d = q <-> p

subseqofsize n = filter ((== 2) . length) . subsequences

groupUnsortedBy pred = groupBy (comb same pred) . sortBy pred
  where
    same = \case EQ -> True; _ -> False
    comb u b x y = u $ b x y

readInput :: String -> (Point, [[(Point, Char)]])
readInput s = (dims, antennas)
  where
    dims = Cell (length . lines $ s) (length . head . lines $ s)
    antennas = groupAntennas . findAntennas $ lines s
    groupAntennas = groupUnsortedBy (compare `on` snd)
    findAntennas = concatMap (filter ((/= '.') . snd)) . zipWithPoint
    zipWithPoint = zipWith (\i row -> zipWith (\j a -> (Point i j, a)) [0 ..] row) [0 ..]

compute :: (Point -> Point -> [Point]) -> [[(Point, Char)]] -> Int
compute gen = length . nub . concatMap (concatMap (\l -> gen (head l) (last l)) . subseqofsize 2 . map fst)

computeA :: Point -> [[(Point, Char)]] -> Int
computeA dim =
  compute
    ( \u v ->
        let d = v <-> u
            onBoard p = p /= u && p /= v && p `within` dim
            around = [u <+> d, u <-> d, v <+> d, v <-> d]
            inside = [u <+> (d </> 3), v <-> (d </> 3)]
         in filter onBoard $ around ++ case d <%> 3 of Point 0 0 -> inside; _ -> []
    )

computeB :: Point -> [[(Point, Char)]] -> Int
computeB dim =
  compute
    ( \u v ->
        let prev = u : map (<-> step u v) prev
            next = u : map (<+> step u v) next
         in takeWhile (`within` dim) prev ++ takeWhile (`within` dim) next
    )

main :: IO ()
main = do
  s <- getContents
  let (dim, l) = readInput s
  print $ computeA dim l
  print $ computeB dim l
