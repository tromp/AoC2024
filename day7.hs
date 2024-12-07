import Data.List.Split

part :: [Integer -> Integer -> Integer] -> String -> Integer
part ops eq = if test `elem` sols then test else 0 where
  (test_: nums_) = words eq
  (test : num0 : nums) = map read (init test_: nums_) :: [Integer]
  sols = foldl tryOps [num0] nums
  tryOps sols num = [op sol num | sol <- sols, op <- ops]

cat x y = read $ show x ++ show y

main = print . sum . map (part [(+),(*),cat]) . lines =<< getContents 
