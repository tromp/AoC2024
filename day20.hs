import Data.List
import Data.Array

n = 141 -- input is n x n map
n1 = n+1
bnds = (0, n*n1-2)

dist mp start = explore maxDist [(start,0)] where
  maxDist = listArray bnds (repeat (maxBound :: Int)) 
  explore dst [] = dst
  explore dst (xs@(x,s):xss) = 
    if s >= dst!x then explore dst xss
    else explore (dst//[xs]) (xss++xss') where
      xss' = [(x',s+1) | x'<-[x+1,x-1,x+n1,x-n1], mp!x' /= '#']

part nc mp = length $ filter (>= 100) saves where
  [Just start, Just end] = map (\c -> findIndex (==c) $ elems mp) "SE"
  mp0 = mp // [(start,'.'),(end,'.')]
  [distS, distE]  = map (dist mp0) [start, end]
  dots = [a | a <- range bnds, mp0!a == '.']
  taxi a b = abs (ax-bx) + abs (ay-by) where [(ay,ax),(by,bx)] = map (`divMod` n1) [a,b]
  cheats = [(a,b) | a <- dots, b <- dots, taxi a b <= nc, distS!b > distS!a]
  saves = [distS!end - (distS!a + taxi a b + distE!b) | (a,b) <- cheats]

main = print . part 20 . listArray bnds =<< getContents
