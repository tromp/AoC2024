import Data.List
import Data.Array

type Node = (Char, Char)

bnds = (('a','a'),('z','z'))

node (a:b:_) = (a,b)
unnode (a,b) = [a,b]

main = do
  input <- getContents
  let edge sx@(_:_:_:sy) = [(x,y),(y,x)] where (x,y) = (node sx, node sy)
  let g = accumArray (flip (:)) [] bnds . (>>= edge) . lines $ input
  let triples = [[x,y,z] | (x,nb) <- assocs g, y <- nb, x < y, z <- nb, y < z, z `elem` g!y]
  print . length . filter ((elem 't') . map fst) $ triples
  let dbl xs = [as++bs | as<-xs, bs<-xs, maximum as < minimum bs, all (\b-> all (`elem` g!b) as) bs]
  let add1 xs = [a:bs | bs<-xs, (a,nb) <- assocs g, a < minimum bs, all (`elem` nb) bs]
  -- we determined experimentlly that a 13-clique is maximum
  mapM_ (putStrLn . intercalate "," . map unnode) . add1 . dbl . dbl $ triples
