import Data.Ix
import Data.Bits
import Data.Array

mask = bit 24 - 1

next s0 = s3 where
  s1 = s0 `xor` ((s0 `shiftL`  6)) .&. mask
  s2 = s1 `xor`  (s1 `shiftR`  5)
  s3 = s2 `xor` ((s2 `shiftL` 11)) .&. mask

pnrg :: Int -> Int -> [Int]
pnrg n s0 = take (n+1) $ iterate next s0

bnds = ((0,0,0,0),(9,9,9,9))

buyerAll (a:b1@(b:c:d:e:_)) = (if inRange bnds db then ((db,e):) else id) $ buyerAll b1 where
  db = (e-a,e-b,e-c,e-d)
buyerAll _ = []

buyerFst = assocs . accumArray (const id) 0 bnds . reverse . buyerAll

main = do
  ss <- getContents >>= return . map (pnrg 2000 . read) . lines
  print . sum $ map last ss
  print . maximum . accumArray (+) 0 bnds . (>>= buyerFst) $ map (map (`mod`10)) ss
