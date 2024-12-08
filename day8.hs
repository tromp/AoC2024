import Data.List
import Data.Function
import qualified Data.Set as S
import Data.Array

n = 50  -- day8.input is n x n letters for city
cityBounds = ((0,0),(n-1,n-1))

part :: [Int] -> Array (Int,Int) Char -> Int
part kRange = S.size . foldl' addAnti S.empty . freqs where
  freqs = map (map fst) . groupBy ((==)`on`snd) . sortBy (compare`on`snd) . filter ((/= '.').snd) . assocs
  addAnti set ants = foldr S.insert set [a2 | a0@(y0,x0) <- ants, a1@(y1,x1) <- ants, a0 /= a1,
     a2 <- takeWhile (inRange cityBounds) $ map (\k->(y0+k*(y1-y0),x0+k*(x1-x0))) kRange]

part1 = [2]
part2 = [1..]

main = getContents >>= print . part part2 . listArray cityBounds . filter (/= '\n')
