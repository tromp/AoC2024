import Data.List

process:: ([Int] -> Int) -> String -> Int
process part = sum . map part . map (map read . words) . lines

safe :: [Int] -> Bool
safe ls = all (\x -> x >= 1 && x <= 3) deltas || all (\x -> x >= -3 && x <= -1) deltas where
  deltas = zipWith (-) (tail ls) ls

part1 :: [Int] -> Int
part1 ls = if safe ls then 1 else 0

rm1 :: [Int] -> [[Int]]
rm1 ls = zipWith (++) (inits ls) (tail (tails ls))

part2 :: [Int] -> Int
part2 ls = if safe ls || any safe (rm1 ls) then 1 else 0

main = getContents >>= print . process part2
