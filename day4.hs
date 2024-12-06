import Data.List
import Data.Array

n = 140  -- day4.input is n x n letters

process:: (Array (Int,Int) Char -> Int) -> String -> Int
process part = part . listArray ((0,0),(n-1,n-1)) . filter (/= '\n')

quads = [[(x,y),(x+1,y),(x+2,y),(x+3,y)] | x <- [0..n-4], y<-[0..n-1]]
     ++ [[(x,y),(x,y+1),(x,y+2),(x,y+3)] | x <- [0..n-1], y<-[0..n-4]]
     ++ [[(x,y),(x+1,y+1),(x+2,y+2),(x+3,y+3)] | x <- [0..n-4], y<-[0..n-4]]
     ++ [[(x,y),(x-1,y+1),(x-2,y+2),(x-3,y+3)] | x <- [3..n-1], y<-[0..n-4]]

part1 :: Array (Int,Int) Char -> Int
part1 ar = length . filter ((\w->w=="XMAS"||w=="SAMX") . map (ar!)) $ quads

xs = [[(x,y),(x,y+2),(x+1,y+1),(x+2,y),(x+2,y+2)] | x <- [0..n-3], y<-[0..n-3]]

part2 :: Array (Int,Int) Char -> Int
part2 ar = length . filter (crossmas . map (ar!)) $ xs where
  crossmas ([xy,xy2,'A',x2y,x2y2]) = all ((== "MS") . sort) [[xy,x2y2], [x2y,xy2]]
  crossmas _ = False

main = getContents >>= print . process part2
