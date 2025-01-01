import Data.List
import Data.List.Split
import Data.Function.Memoize

poss :: [String] -> [Int]
poss (atoms_:_:words) = map (memoFix npossible) words where
  atoms = splitOn ", " atoms_
  npossible memo "" = 1
  npossible memo w = sum $ [memo (drop (length a) w) | a <- atoms, a `isPrefixOf` w]

part1 = length . filter (> 0)
part2 = sum

main = print . part2 . poss . lines =<< getContents
