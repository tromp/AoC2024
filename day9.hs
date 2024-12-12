import Data.Char
import Data.Array

n = 10000-- input is n files with (n-1) gaps in between

-- positions, sizes
type Disk = (Array Int Int, Array Int Int)

process:: (Disk -> Int) -> String -> Int
process part = part . mkDisk . map digitToInt . filter (/= '\n') where
  mkDisk ints = (poses, sizes) where
    sizes = listArray (0,2*n-2) ints
    poses = listArray (0,2*n-1) $ scanl (+) 0 ints

-- check sum contribution from filling n positions pos...pos+n-1 with file id
fill :: Int -> Int -> Int -> Int
fill pos n id = id * (n*pos + n*(n-1)`div`2)

part1 :: Disk -> Int
part1 (_,disk) = go 0 0 0 0 0 n where
  -- gap is remaining empty blocks before file id f0
  -- blk is remaining file blocks of file id f1
  go chksum pos gap f0 blk f1 | f0 >= f1 = chksum + fill pos blk f1
  go chksum pos gap f0   0 f1 =  go chksum  pos  gap  f0 (disk!(2*f1-2)) (pred f1) -- new block
  go chksum pos   0 f0 blk f1 =  go chksum' pos' gap' (f0+1) blk f1 where -- new gap
    n = disk!(2*f0)
    pos' = pos + n
    chksum' = chksum + fill pos n f0
    gap' = disk!(2*f0+1)
  go chksum pos gap f0 blk f1 =  go (chksum+fill pos n f1) (pos+n) (gap-n) f0 (blk-n) f1 where
    n = min gap blk -- reduce gap

part2 :: Disk -> Int
part2 (poses,sizes) = go 0 sizes n where
  go chksum gaps 0 = chksum
  go chksum gaps fid = go chksum' gaps' fid' where
    fid' = fid - 1
    n = sizes!(2*fid')
    i = length . takeWhile (< n) . map (gaps!) $ [1,3..2*fid'+1]
    gap = gaps!(2*i+1)
    (pos',gaps') = if i < fid'
      then (poses!(2*(i+1)) - gap, gaps//[(2*i+1,gap-n)])
      else (poses!(2*fid')       , gaps)
    chksum' = chksum + fill pos' n fid'

main = getContents >>= print . process part2
