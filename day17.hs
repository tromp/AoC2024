import Data.List
import Data.Bits
import Data.Array

data Opcode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Eq,Enum)

proglist = [2,4,  1,2,  7,5,    4,7,  1,3,  5,5,  0,3,  3,0]
--          BST   BXL   CDV     BXC   BXL   OUT   ADV   JNZ
--          B=A&7 B^=2  C=A>>B  B^=C  B^=3  PR B  A>>=3
--          B=A&7^2;    B ^ A>>B ^ 3 & 7;     A>>=3

proglen = length proglist

runa a = run (a,0,0) 0 where
  prog = listArray (0,proglen-1) proglist
  run (a,b,c) ip = let
      ip' = ip + 2
      lit = prog!(ip+1)
      combo = case lit of { 4 -> a; 5 -> b; 6 -> c; _ -> lit }
    in if ip >= proglen then []
      else case toEnum (prog!ip) of
        ADV -> run (a`shiftR`combo,b,c) ip'
        BXL -> run (a,b`xor`lit,c) ip'
        BST -> run (a,combo.&.7,c) ip'
        JNZ -> run (a,b,c) (if a==0 then ip' else lit)
        BXC -> run (a,b`xor`c,c) ip'
        OUT -> (combo.&.7) : run (a,b,c) ip'
        BDV -> run (a,a`shiftR`combo,c) ip'
        CDV -> run (a,b,a`shiftR`combo) ip'

part1 = putStrLn . intercalate "," . map show $ runa 35200350 where

invert p a = [a' | a0 <- [0..7], let a' = 8*a+a0, p `xor` a0 `xor` (a' `shiftR` (a0`xor`2)) .&. 7 == 1]

part2 = print . minimum $ foldr (\p -> (>>= invert p)) [0] proglist

main = part2
