import Data.List

getmul :: String -> Int
getmul ('m':'u':'l':s) = case reads s of
  [((x,y),_)]   -> x * y
  otherwise -> 0
getmul _ = 0

part1 :: String -> Int
part1 = sum . map getmul . tails

go :: Int -> Bool -> String -> Int
go m _ [] = m
go m _ ('d':'o':'(':')':s) = go m True s
go m _ ('d':'o':'n':'\'':'t':'(':')':s) = go m False s
go m True s@(_:s') = getmul s + go m True s'
go m b (_:s) = go m b s

part2 :: String -> Int
part2 = go 0 True

main = getContents >>= print . part2
