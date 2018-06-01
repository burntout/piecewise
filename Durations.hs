module Durations where

type Duration = Double

bpm = 140

triplet d = 2 * d / 3 
dot d = 3 * d / 2 

n1 = 2 * n2
n2 = 2 * n4
n4 = 60/bpm
n8 = n4/2
n16 = n8/2



n4t = triplet n4
n8t = triplet n8
n16t = triplet n16


n2d = dot n2
n4d = dot n4
n8d = dot n8


