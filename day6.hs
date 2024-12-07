import Data.List
import Data.Maybe
import Data.Array

n = 130  -- day6.input is n x n letters of Lab area
labBounds = ((0,0),(n-1,n-1))

type Lab = Array (Int,Int) Char 

process:: (Lab -> Int) -> String -> Int
process part = part . listArray labBounds  . filter (/= '\n')

dirs = "^>v<"

rotate :: Char -> Char
rotate '^' = '>'
rotate '>' = 'v'
rotate 'v' = '<'
rotate '<' = '^'

step :: Char -> (Int,Int) -> (Int, Int)
step '^' (y,x) = (y-1,x)
step '>' (y,x) = (y,x+1)
step 'v' (y,x) = (y+1,x)
step '<' (y,x) = (y,x-1)

findGuard :: Lab -> ((Int,Int), Char)
findGuard = fromMaybe (error "no guard") . find ((`elem` dirs) . snd) . assocs

visited :: Lab -> [(Int,Int)]
visited lab = map fst . filter ((== ':') . snd) . assocs . patrol lab . findGuard $ lab where
  patrol :: Lab -> ((Int,Int),Char) -> Lab
  patrol lab (yx, dir) = let
    lab' = lab // [(yx,':')]
    yx' = step dir yx
   in if not (inRange labBounds yx') then lab'
      else patrol lab' $ if lab!yx' == '#' then (yx, rotate dir) else (yx', dir)
  countVisited = length . filter (== ':') . elems

part1 :: Lab -> Int
part1 = length . visited

part2 :: Lab -> Int
part2 lab = length [yx | yx<-visited lab, yx /= gyx,
                         loops (lab // [(yx,'#'),(gyx,'.')]) guard] where
  guard@(gyx,_) = findGuard lab
  loops lab (yx, dir) = let yx' = step dir yx
    in if not (inRange labBounds yx') then False
       else if lab!yx' /= '#' then loops lab (yx', dir)
       else if lab!yx `elem` [dir,rotate dir] then True
       else loops (lab//[(yx,dir)]) (yx, rotate dir)

main = getContents >>= print . process part2
