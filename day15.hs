import Data.List
import Data.Array
import Data.List.Split

n = 50 -- input is n x n map

part widen [mps, mvs] = sum . map gps . assocs . foldl move mp0 . concat $ mvs where
  n1 = length (widen '.')*(n+1)  -- row length including newlines
  bnds = (0, n*n1-2)
  mp0 = listArray bnds $ unlines mps >>= widen
  move mp c = mv (mp//[(i,'.')]) [(i+d,'@')] where
    Just i = findIndex (=='@') $ elems mp
    Just (d,vert) = lookup c $ zip "<>^v" [(-1,False),(1,False),(-n1,True),(n1,True)]
    mv m [] = m
    mv m ((j,c):js) = if m!j=='#' then mp else mv (m//m') (js++js') where
      (m',js') = case m!j of
         '.' -> ([(j,c)], [])
         'O' -> ([(j,c)], [(j+d,'O')])
         '[' -> ([(j,c)]++[(j+1,'.')|vert], [(j+d,'[')]++[(j+d+1,']')|vert])
         ']' -> ([(j-1,'.')|vert]++[(j,c)], [(j+d-1,'[')|vert]++[(j+d,']')])
  gps (i,c) = if c/='O' && c/='[' then 0 else 100*y + x where (y,x) = i `divMod` n1

widen1 = (:[])
widen2 'O' = "[]"
widen2 '@' = "@."
widen2  c  = [c,c]

main = print . part widen2 . splitOn [""] . lines =<< getContents
