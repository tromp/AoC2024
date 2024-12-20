import Data.List
import Control.Monad
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

n = 140 -- input is n x n map
bnds = ((0,0), (n-1,n-1))

tuplus (x0,y0) (x1,y1) = (x0+x1,y0+y1)

type Map = Array (Int,Int) Char

uniq :: Map -> Map
-- unique region labels
uniq mp = snd . foldl relabel ('a',mp) $ indices mp where
  relabel :: (Char, Map) -> (Int,Int) -> (Char, Map)
  relabel zar@(z,ar) yx =
    if ar!yx > 'Z' then zar
    else (succ z, ar//map (,z) (fill S.empty [yx])) where
      fill done [] = []
      fill done (yx:tl) = if yx `S.member` done then fill done tl
                          else yx: fill (S.insert yx done) ([yx' | dir <- [(0,1),(1,0),(0,-1),(-1,0)], let yx' = tuplus yx dir, inRange bnds yx', ar!yx' == ar!yx] ++ tl)
    
part fn mp = sum . map price . elems $ regions where
  regions = accumArray tuplus (0,0) (minimum mp, maximum mp) $ do
     i <- [0..n]
     j <- [0..n]
     let onmap yx = if inRange bnds yx then mp!yx else ' '
     let sq = map onmap $ [(i-1,j-1),(i-1,j),(i,j),(i,j-1)]
     ([left,ahead,opp,right],dlta) <- zip (tail $ zipWith (++) (tails sq) (inits sq)) [-j,-i,j,i]
     guard $ left /= right && left /= ' '
     [(left, (fn left ahead opp, dlta))]
  price = (`div` 2) . abs . uncurry (*)

part1 _ _ _ = 1
part2 left ahead opp = if left/=ahead||left==opp then 1 else 0

main = getContents >>= print . part part2 . uniq . listArray bnds . filter (/= '\n')
