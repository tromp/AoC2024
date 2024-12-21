import Data.List.Split

readXY :: String -> (Int, Int)
readXY ln = (x,y) where
  [_,xs,_,ys] = splitOneOf "+=," ln
  [x,y] = map read [xs,ys]

part delta [(a,c),(b,d),(p_,q_)] = if det == 0 || (mA+mB) /= 0 then 0 else 3*nA + nB where
  (p,q) = (p_ + delta, q_ + delta)
  [(nA,mA),(nB,mB)] = map (`divMod` det)  [(d*p-b*q), (a*q-c*p)]
  det = a*d - b*c

main = print . sum . map (part 10000000000000 . map readXY) . splitOn [""] . lines =<< getContents
