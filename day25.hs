import Data.List
import Data.Tuple.Extra
import Data.List.Split

part1 lockeys = length [1 | lock <- locks, key <- keys, fits lock key] where
  histo ls@((c:_):_) = (c=='#', map (length . filter (=='#')) . transpose $ ls)
  (locks,keys) = both (map snd) . partition fst . map histo $ lockeys
  fits lock key = all (<= 7) $ zipWith (+) lock key

main = getContents >>= print . part1 . splitOn [[]] . lines
