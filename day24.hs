import Data.List
import Data.Bits

x00 = 1
x01 = 1
x02 = 1
x03 = 1
x04 = 0
x05 = 1
x06 = 0
x07 = 1
x08 = 0
x09 = 1
x10 = 1
x11 = 1
x12 = 1
x13 = 1
x14 = 1
x15 = 0
x16 = 1
x17 = 1
x18 = 1
x19 = 0
x20 = 0
x21 = 0
x22 = 1
x23 = 0
x24 = 1
x25 = 1
x26 = 0
x27 = 1
x28 = 1
x29 = 1
x30 = 0
x31 = 1
x32 = 1
x33 = 1
x34 = 1
x35 = 1
x36 = 1
x37 = 0
x38 = 0
x39 = 1
x40 = 1
x41 = 0
x42 = 1
x43 = 1
x44 = 1
y00 = 1
y01 = 1
y02 = 1
y03 = 1
y04 = 1
y05 = 0
y06 = 0
y07 = 0
y08 = 0
y09 = 0
y10 = 1
y11 = 0
y12 = 0
y13 = 1
y14 = 1
y15 = 0
y16 = 1
y17 = 0
y18 = 0
y19 = 0
y20 = 1
y21 = 0
y22 = 1
y23 = 0
y24 = 0
y25 = 0
y26 = 1
y27 = 1
y28 = 1
y29 = 1
y30 = 1
y31 = 1
y32 = 1
y33 = 0
y34 = 1
y35 = 0
y36 = 1
y37 = 1
y38 = 1
y39 = 0
y40 = 0
y41 = 1
y42 = 1
y43 = 1
y44 = 1

bcs = x09 .&. y09
bdf = tqd .&. pmd
bdk = x00 .&. y00
bjg = ccs .|. qck
bmh = x32 .&. y32
bnc = cdn .|. qdr
bqd = y10 .&. x10
bwj = y39 `xor` x39
ccs = x34 .&. y34
cdj = tck .&. ddg
cdn = jtv .&. nsc
cfr = x25 `xor` y25
cgh = hbd .|. cst
chf = nrf .|. jkk
chr = x39 .&. y39
cmk = hvv .&. rnq
cpf = y02 .&. x02
cqv = vvd .|. dqh
cst = x11 .&. y11
ddg = kqj .|. bcs
dgf = y07 .&. x07
dhq = x31 .&. y31
djk = y14 `xor` x14
dkb = ttw .|. ggw
dnv = jbj .&. rkg
dqh = y36 .&. x36
dsq = bdf .|. dgf
dsw = x06 .&. y06
dtn = y28 `xor` x28
dvq = cqv .&. gvf
fcd = smf `xor` rfd
fdg = y12 .&. x12
fgp = mtq .&. tkj
fhn = x23 .&. y23
fhp = tsc `xor` pns
fjg = x35 .&. y35
fmg = y30 `xor` x30
fmr = hmk .|. ndj
fnj = x28 .&. y28
fnv = fgp .|. qkp
fnw = ftg .|. tvw
fpf = tnp .&. rft
ftg = y43 .&. x43
gdq = x19 `xor` y19
ggg = ncf .|. vcj
ggw = pbf .&. qwr
gjj = wjq .&. thj
gkb = y11 `xor` x11
gkk = jct .&. hmm
gms = spt .&. vjn
grr = x40 `xor` y40
gvf = x37 `xor` y37
gvh = y05 .&. x05
hbc = y31 `xor` x31
hbd = gkb .&. pjh
hdc = gkk .|. njd
hhc = x32 `xor` y32
hmk = vmr `xor` bnc
hmm = y41 `xor` x41
hnr = ttv .|. mbp
hrk = njv .&. shm
hvv = y23 `xor` x23
hwg = pwm .&. djk
jbj = cmk .|. fhn
jbk = x05 `xor` y05
jct = nmp .|. jjg
jcw = ggg .&. prd
jgr = rfd .&. smf
jhb = x04 .&. y04
jjg = grr .&. svt
jkg = fmr .&. vtd
jkk = y18 .&. x18
jnh = rsq .&. bdk
jtv = y15 `xor` x15
kbt = y42 `xor` x42
kgb = x17 .&. y17
kkp = mgm .&. cfr
knm = qvp .|. dhq
kqj = pwv .&. sqm
krw = x21 `xor` y21
ksv = fnj .|. wsm
kvm = fnw .&. wcj
mbp = tmq .&. fmg
mch = sjc .|. jhb
mfv = scf .&. ksv
mgc = x34 `xor` y34
mgm = dnv .|. pbd
mpw = tqw .&. cgh
mtq = y03 `xor` x03
mvn = x18 `xor` y18
nbj = x08 .&. y08
ncf = x21 .&. y21
ndj = bnc .&. vmr
njd = y41 .&. x41
njv = fdg .|. mpw
nmd = y42 .&. x42
nmp = y40 .&. x40
nps = wgr .&. bwj
nrf = mvn .&. wjt
nsc = wbg .|. hwg
ntn = tpc .|. rmv
pbd = y24 .&. x24
pbf = trg .|. kkp
pgp = x29 .&. y29
pjh = cdj .|. bqd
pmd = x07 `xor` y07
pns = x20 `xor` y20
prd = x22 `xor` y22
pwm = hrk .|. rnb
pwv = nbj .|. tqv
qck = fcd .&. mgc
qdr = y15 .&. x15
qgq = y19 .&. x19
qkj = x01 .&. y01
qkp = y03 .&. x03
qmw = bjg .&. qrh
qpp = chf .&. gdq
qrh = x35 `xor` y35
qvp = hbc .&. hnr
qwr = y26 `xor` x26
rfd = bmh .|. stm
rft = dvq .|. ssp
rhc = x36 `xor` y36
rkg = x24 `xor` y24
rmv = dkb .&. rvf
rnb = x13 .&. y13
rnq = jcw .|. wnk
rsq = y01 `xor` x01
rtc = fjg .|. qmw
rvf = x27 .&. y27
rwm = x20 .&. y20
scf = y29 `xor` x29
sfs = x38 .&. y38
shm = y13 `xor` x13
sjc = wfr .&. fnv
smf = x33 `xor` y33
spt = jnh .|. qkj
sqm = x09 `xor` y09
ssp = y37 .&. x37
stm = hhc .&. knm
svd = y44 .&. x44
svt = chr .|. nps
tbw = y08 `xor` x08
tck = x10 `xor` y10
tcw = mch .&. jbk
thj = y06 `xor` x06
tkj = gms .|. cpf
tmq = pgp .|. mfv
tnp = x38 `xor` y38
tpc = y27 `xor` x27
tqd = gjj .|. dsw
tqv = dsq .&. tbw
tqw = y12 `xor` x12
trg = y25 .&. x25
tsc = qgq .|. qpp
ttv = x30 .&. y30
ttw = y26 .&. x26
tvw = vbv .&. vcd
vbv = y43 `xor` x43
vcd = nmd .|. wdk
vcj = krw .&. vnk
vjn = y02 `xor` x02
vmr = y16 `xor` x16
vnk = fhp .|. rwm
vtd = y17 `xor` x17
vvd = rtc .&. rhc
wbg = y14 .&. x14
wcj = y44 `xor` x44
wdk = hdc .&. kbt
wfr = x04 `xor` y04
wgr = sfs .|. fpf
wjq = tcw .|. gvh
wjt = jkg .|. kgb
wkw = x33 .&. y33
wnk = y22 .&. x22
wsm = ntn .&. dtn
z00 = y00 `xor` x00 --
z01 = bdk `xor` rsq
z02 = spt `xor` vjn
z03 = mtq `xor` tkj
z04 = wfr `xor` fnv
z05 = mch `xor` jbk
z06 = wjq `xor` thj
z07 = pmd `xor` tqd
z08 = tbw `xor` dsq
z09 = sqm `xor` pwv
z10 = ddg `xor` tck
z11 = gkb `xor` pjh
z12 = cgh `xor` tqw
z13 = shm `xor` njv
z14 = djk `xor` pwm
z15 = jtv `xor` nsc
z16 = y16 .&. x16
z17 = fmr `xor` vtd
z18 = mvn `xor` wjt
z19 = gdq `xor` chf
z20 = pns .&. tsc
z21 = krw `xor` vnk
z22 = prd `xor` ggg
z23 = rnq `xor` hvv
z24 = jbj `xor` rkg
z25 = mgm `xor` cfr
z26 = qwr `xor` pbf
z27 = rvf `xor` dkb
z28 = dtn `xor` ntn
z29 = scf `xor` ksv
z30 = tmq `xor` fmg
z31 = hbc `xor` hnr
z32 = hhc `xor` knm
z33 = wkw .|. jgr
z34 = fcd `xor` mgc
z35 = qrh `xor` bjg
z36 = rtc `xor` rhc
z37 = gvf `xor` cqv
z38 = rft `xor` tnp
z39 = wgr `xor` bwj
z40 = svt `xor` grr
z41 = jct `xor` hmm
z42 = hdc `xor` kbt
z43 = vcd `xor` vbv
z44 = wcj `xor` fnw
z45 = kvm .|. svd

inX = [x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44]
inY = [y00, y01, y02, y03, y04, y05, y06, y07, y08, y09, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44]

outs :: [Int]
outs = [z00, z01, z02, z03, z04, z05, z06, z07, z08, z09, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z24, z25, z26, z27, z28, z29, z30, z31, z32, z33, z34, z35, z36, z37, z38, z39, z40, z41, z42, z43, z44, z45]

main = do
  print $ foldr (\a b-> a+2*b) 0 inX
  print $ foldr (\a b-> a+2*b) 0 inY
  print $ foldr (\a b-> a+2*b) 0 outs
  putStrLn "fcd,fhp,hmk,rvf,tpc,z16,z20,z33"