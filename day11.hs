import Data.List
import Data.Function

sumtuples :: [(Integer,Integer)] -> (Integer,Integer)
sumtuples = foldl1 (\(x,n0) (_,n1) -> (x,n0+n1))

reduce :: [(Integer,Integer)] -> [(Integer,Integer)]
reduce = map sumtuples . groupBy ((==) `on` fst) . sort

splits :: [(Integer,Integer)] -> [(Integer,Integer)]
splits xs = reduce $ do
  (x,n) <- xs
  let
    showx = show x
    lenshowx = length showx
   in if x==0 then return (1,n)
      else if even lenshowx then let (l,r) = splitAt (lenshowx `div` 2) showx in map ((,n).read) [l,r]
      else return (x*2024,n)

main = print . sum . map snd . (!!75) . iterate splits . map ((,1).read) . words =<< getContents
