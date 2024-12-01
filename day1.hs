import Data.List
import qualified Data.Set as S

process:: ([[Int]] -> [Int]) -> String -> Int
process part = sum . part . transpose . map (map read . words) . lines

part1 :: [[Int]] -> [Int]
part1 [as,bs] = zipWith (\x y -> abs (x-y)) (sort as) (sort bs) where

part2 :: [[Int]] -> [Int]
part2 [as,bs] = filter (`elem` S.fromList as) bs

main = getContents >>= print . process part2
