import Data.List
import Data.List.Split
import Data.Array

n = 71 -- input is n x n map
n1 = n+1
bnds = (-n1, n1*n1-2)
(start, end) = (0, n*n1-2)

readmap :: [String] -> Array Int Char
readmap = array bnds . ((empty++border)++) . map block where
  block s = let [x,y] = map read $ splitOn "," s in (x+y*n1,'#')
  empty  = map (,'.') $ [0..n*n1-2]
  border = map (,'#') $ [-n1.. -2] ++ [-1,n..n*n1-1] ++ [n*n1..n1*n1-2]

dist mp start = explore maxDist [(start,0)] where
  maxDist = listArray bnds (repeat (maxBound :: Int)) 
  explore dst [] = dst
  explore dst (xs@(x,s):xss) = 
    if s >= dst!x then explore dst xss
    else explore (dst//[xs]) (xss++xss') where
      xss' = [(x',s+1) | x'<-[x+1,x-1,x+n1,x-n1], mp!x' /= '#']

part1 ls = show $ dist (readmap $ take 1024 ls) start ! end

part2 ls = unreach 0 (length ls) where
  unreach n0 n1 | n1 == succ n0 = ls!!n0
  unreach n0 n1 = if (dist (readmap $ take nmid ls) start)!end == maxBound
                  then unreach n0 nmid else unreach nmid n1 where
    nmid = (n0 + n1) `div` 2

main = putStrLn . part2 . lines =<< getContents
