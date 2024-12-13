import Data.List
import Data.Array

n = 40 -- input is n x n map
n1 = n+1  -- row length including newline
bnds = (0, n*n1-2)

type Map = Array Int Char

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

part :: ([Int] -> [Int]) -> Map -> Int
part trails mp = sum . map score $ trailheads where
  trailheads = map fst . filter ((== '0').snd) . assocs $ mp
  score th = length $ foldl expand [th] ['1'..'9']
  expand xs h = trails [nb | x <- xs, dir <- [-n1,-1,1,n1], let nb = x+dir, inRange bnds nb, mp!nb == h]

part1 = part uniq
part2 = part id

main = getContents >>= print . part2 . listArray bnds
