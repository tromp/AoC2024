import Data.List
import Data.Array
import Data.Function

n = 141 -- input is n x n map
n1 = n+1
bnds = (0, n*n1-2)
dbnds = ((0,False),(n*n1-2,True))

dist mp start = explore maxDist [(start,0)] where -- False for facing horizontally; True for vertically
  maxDist = listArray dbnds (repeat (maxBound :: Int)) 
  explore dst [] = dst
  explore dst (xvs@(xv@(x,vert),s):xs) = 
    if s >= dst!xv then explore dst xs
    else explore (dst//[xvs]) (xs++xs') where
      xs' = ((x,not vert),s+1000) : [((x',vert),s+1) | x'<-[x+dir,x-dir], mp!x' /= '#']
      dir = if vert then n1 else 1

part1 mp = (dist mp (start,False))!(end,True) where
  [Just start, Just end] = map (\c -> findIndex (==c) $ elems mp) "SE"

part2 mp = length . filter (\x->opt (x,False) || opt (x,True)) . range $ bnds where
  [Just start, Just end] = map (\c -> findIndex (==c) $ elems mp) "SE"
  [distS,distE]  = map (dist mp) [(start,False), (end,True)]
  opt xv = distS!xv+distE!xv == distS!(end,True) 

main = print . part2 . listArray bnds =<< getContents
