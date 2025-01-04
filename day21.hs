import Data.Array

pad = "789456123?EABCD"
--               ^A<v>
coordBnds = ((0,0),(4,2))
coords = range coordBnds

padCoords = array ('0','E') $ ('0', padCoords!'E'): zip pad coords -- overlay '0' on 'E' (up)
padKeys   = array coordBnds $ zip coords pad

paths :: [((Char,Char),Int)] -> [((Char,Char),Int)]
paths cs = assocs . accumArray (+) 0 (('A','A'),('E','E')) $ cs >>= path where
  path ((from,to),n) = map (,n) $ zip ('A':keys) (keys++['A']) where
    keys = if padKeys!(toY,fromX)=='?' || padKeys!(fromY,toX)/='?' && dX<0 then mvX++mvY else mvY++mvX
    [(fromY,fromX),(toY,toX)] = map (padCoords!) [from,to]
    (dY,dX) = (toY-fromY, toX-fromX)
    mvY = replicate (abs dY) (if dY > 0 then 'C' else 'E')
    mvX = replicate (abs dX) (if dX > 0 then 'D' else 'B')

part2 :: String -> Int
part2 s = (read (init s)) * (sum . map snd . (!!26) . iterate paths . map (,1) $ (zip ('A':s) s))

main = print . sum . map part2 . lines =<< getContents where
