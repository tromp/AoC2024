import Data.List
import Data.List.Split
import qualified Data.Set as S

readEdge :: String -> (Int, Int)
readEdge edge = (x,y) where [x,y] = map read . splitOn "|" $ edge

readUpdate :: String -> [Int]
readUpdate = map read . splitOn ","

type CmpPage = Int -> Int -> Ordering

ordered :: CmpPage -> [Int] -> Bool
ordered cmpPage update@(_:update1) = all ((==LT) . uncurry cmpPage) $ zip update update1

part1 :: CmpPage -> [Int] -> Int
part1 cmpPage update = if ordered cmpPage update then update!!(length update `div` 2) else 0

part2 :: CmpPage -> [Int] -> Int
part2 cmpPage update = if ordered cmpPage update then 0
  else (sortBy cmpPage update) !! (length update `div` 2)

main = do
  [cmpPageLines,updateLines] <- return . splitOn [[]] . lines =<< getContents 
  let edges = S.fromList (map readEdge cmpPageLines)
  let cmpPage a b = if (a,b) `S.member` edges then LT else GT
  print . sum . map (part2 cmpPage) . map readUpdate $ updateLines
