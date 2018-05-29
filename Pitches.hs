module Pitches where

type Pitch = Double

transpose:: Double -> Pitch -> Pitch
transpose i p = 2**(i/12) * p

a0 = 27.5
bf0 = transpose 1 a0
b0  = transpose 2 a0
c1  = transpose 3 a0
cs1 = transpose 4 a0
d1  = transpose 5 a0
ef1 = transpose 6 a0
e1  = transpose 7 a0
f1  = transpose 8 a0
fs1 = transpose 9 a0
g1  = transpose 10 a0
af1  = transpose 11 a0

a1 = 55.0
bf1 = transpose 1 a1
b1  = transpose 2 a1
c2  = transpose 3 a1
cs2 = transpose 4 a1
d2  = transpose 5 a1
ef2 = transpose 6 a1
e2  = transpose 7 a1
f2  = transpose 8 a1
fs2 = transpose 9 a1
g2  = transpose 10 a1
af2  = transpose 11 a1

a2 = 110.0
bf2 = transpose 1 a2
b2  = transpose 2 a2
c3  = transpose 3 a2
cs3 = transpose 4 a2
d3  = transpose 5 a2
ef3 = transpose 6 a2
e3  = transpose 7 a2
f3  = transpose 8 a2
fs3 = transpose 9 a2
g3  = transpose 10 a2
af3  = transpose 11 a2

a3 = 220.0
bf3 = transpose 1 a3
b3  = transpose 2 a3
c4  = transpose 3 a3
cs4 = transpose 4 a3
d4  = transpose 5 a3
ef4 = transpose 6 a3
e4  = transpose 7 a3
f4  = transpose 8 a3
fs4 = transpose 9 a3
g4  = transpose 10 a3
af4  = transpose 11 a3

a4 = 440.0
bf4 = transpose 1 a4
b4  = transpose 2 a4
c5  = transpose 3 a4
cs5 = transpose 4 a4
d5  = transpose 5 a4
ef5 = transpose 6 a4
e5  = transpose 7 a4
f5  = transpose 8 a4
fs5 = transpose 9 a4
g5  = transpose 10 a4
af5  = transpose 11 a4

a5  = 880.0
bf5 = transpose 1 a5
b5  = transpose 2 a5
c6  = transpose 3 a5
cs6 = transpose 4 a5
d6  = transpose 5 a5
ef6 = transpose 6 a5
e6  = transpose 7 a5
f6  = transpose 8 a5
fs6 = transpose 9 a5
g6  = transpose 10 a5
af6  = transpose 11 a5

a6  = 1760.0
bf6 = transpose 1 a6
b7  = transpose 2 a6
c7  = transpose 3 a6
cs7 = transpose 4 a6
d7  = transpose 5 a6
ef7 = transpose 6 a6
e7  = transpose 7 a6
f7  = transpose 8 a6
fs7 = transpose 9 a6
g7  = transpose 10 a6
af7  = transpose 11 a6



