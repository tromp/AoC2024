import Data.List
import Data.Maybe
import Data.Array
import Data.List.Split
import CRT

(w,h) = (101, 103)
(w2, h2) = (w`div`2, h`div`2)

readBot s = ((x0,y0),(vx,vy)) where
  [_,x0_,y0_,_,vx_,vy_] = splitOneOf "=, " s
  [x0,y0,vx,vy] = map read [x0_,y0_,vx_,vy_]

step i ((x0,y0),(vx,vy)) = ((x0 + i*vx) `mod` w, (y0 + i*vy) `mod` h)

part1 = product . elems . accumArray (+) 0 (0,3) . map (quadrant . step 100) where
  quadrant (x,y) = (2 * (x`div`(w2+1)) + y`div`(h2+1), if x==w2 || y==h2 then 0 else 1)

part2 bots = crt [(align fst, w), (align snd, h)] where
  align coord = fromIntegral . fromJust . findIndex (> 30) $ map treelike [0..max w h - 1] where
    treelike i = maximum . map length . group . sort . map (coord . step i) $ bots

main = getContents >>= print . part2 . map readBot . lines
