module Patterns where
-- import Constants
import Data.Int
import Data.WAVE
import System.Process
import System.Random
import Pitches
import Voices
import VoiceWaves


type Pattern = [Note]
data Note = Note { pitch :: (Maybe Pitch), voice :: (Maybe Voice), duration :: (Maybe Float) } deriving (Show)

riff0 = concat $ replicate 4  $ map (applyEnvelope env1) $ map (\pitch -> genWobbleWave pitch 0.25) $  (map transpose [0, 5, 7, 10, 0, 5, 7, 12]) <*> [a3]
riff1 = concat $ replicate 4  $ map (applyEnvelope env1) $ map (\pitch -> genWobbleWave pitch 0.125) $  (map transpose [0, 5, 7, 10, 0, 5, 7, 12]) <*> [a3]
riff2 = concat $ replicate 4  $ map (applyEnvelope env1) $ map (\pitch -> genSquareWave pitch 0.125) $  (map transpose [0, 5, 7, 10, 0, 5, 7, 12]) <*> [a2]
riff3 = concat $ replicate 4  $ map (applyEnvelope env2) $ map (\pitch -> genSquareWave pitch 0.125) $  (map transpose [0, 5, 7, 10, 0, 5, 7, 12]) <*> [a2]
riff4 = concat $ replicate 4  $ map (applyEnvelope env2) $ map (\pitch -> genSquareWave pitch 0.125) $  (map transpose [0, 5, 7, 10, 0, 5, 7, 12]) <*> [a1]

riffWav = genWaveFile $  riff0 ++ riff0 ++ riff1 ++ riff2 ++ riff3 ++ riff4 ++ riff4 

arp1arp = map transpose [0, 5, 7, -5, 0, 5, 7, -2]
arp0 = arp1arp <*> [a1, a1, a2, a2]
arp1 = arp1arp <*> (arp1arp <*> [a1, a1, a2, a2])

parp = map transpose [0, 12, 3, 15, 5, 17, -2, 10]
parp0 = parp <*> [a2]
parp1 = parp <*> parp0

pmess0  = map (applyEnvelope env4) $ [\p -> genDistSaw p 0.125, \p -> genDistOrgan p 0.125] <*> parp0
pmess2  = map (applyEnvelope env4) $ [\p -> genDistSaw p 0.125, \p -> genDistOrgan p 0.125] <*> (map (transpose (-12)) parp0)
pmess1  = map (applyEnvelope env4) $ [\p -> genDistSaw p 0.125, \p -> genDistOrgan p 0.125] <*> parp1
pmess1wav = genWaveFile $ pmess0 ++ pmess0 ++ pmess1 ++ pmess0 ++ pmess2 

-- stab0 = map (applyEnvelope env3) $ map (\p -> genMinorSaw2 p 0.125) [a3, a3, a3, a2, d3, d3, e2, g2 ]
stab0 = map (applyEnvelope env3) $ map (\p -> genMinorSaw2 p 0.125) $ (map transpose [12, 12, 12, 0, 5, 5, 7, -2]) <*> [a3]
stab1 = map (applyEnvelope env3) $ map (\p -> genMinorSquare p 0.125) $ (map transpose [12, 12, 12, 0, 5, 5, 7, -2]) <*> [a3]
stab2 = map (applyEnvelope env1) $ map (\p -> genMinorSaw2 p 0.25) $ (map transpose [12, 12, 12, 0]) <*> [a3]
stab3 = map (applyEnvelope env1) $ map (\p -> genMinorSaw2 p 0.125) $ concat $ replicate 2 $ (map transpose [12, 12, 12, 0]) <*> [a3]
-- stab0 = map (\p -> genMinorSquare p 0.5) [a4, c5]
stab0wav = genWaveFile $ stab0 ++ stab1 ++ stab2 ++ stab3

-- riley = map transpose [0, 1, 4, 5, 7, 8, 10]
-- riley = map transpose [-10, 0, 1, 4, 5, 7, 8]
riley = map transpose [0, 2, 4, 5, 7, 9, 11, 12]
riley0 = riley <*> [a1, a2]
riley1 = riley <*> [a2, a3] 
riley2 = riley <*> [a3, d4]
riley3 = riley <*> [a4, d5, e5, a5]
riley4 = riley <*> [a5, d4]

arp2 = map (transpose (-12)) arp0
arp3 = map (transpose (-24)) arp0

tones0 = map (\pitch -> genSquareWave pitch 0.125) arp0
tones1 = map (\pitch -> genPWSquareWave pitch 0.125) arp1
ptones0 = map (\pitch -> genPluckWave pitch 0.125) arp0
stones1 = map (\pitch -> genSaw3Wave pitch 0.125) arp1
stones0 = map (\pitch -> genSaw3Wave pitch 0.125) arp0
ptones1 = map (\pitch -> genPluckWave pitch 0.125) arp1

dotones1 = map (applyEnvelope env4) $ map (\pitch -> genDistOrgan pitch 0.125) arp1
dotones0 = map (applyEnvelope env4) $ map (\pitch -> genDistSaw pitch 0.125) arp0



tones2 = map (applyEnvelope env3) tones1
tones3 = map (applyEnvelope env3) tones0

rtones = map (\pitch -> genSaw3Wave pitch 0.125) $ riley0 ++ riley1 ++ riley2 ++ riley3 ++ riley4 ++ (reverse (riley0 ++ riley1 ++ riley2 ++ riley3 ++ riley4))

mnoise0 = map (applyEnvelope env3) (map (\pitch -> genMetalNoise pitch 0.125) (arp0 ++ arp2 ++ arp3))
mnoise1 = map (\pitch -> genMetalNoise pitch 0.125) (arp0 ++ arp2)

sn20 = genSn 20 1
sn30 = genSn 30 1
sn40 = genSn 40 1
sn50 = genSn 50 1
sn60 = genSn 60 1
sn70 = genSn 70 1

seq0 = [applyFadeIn fadeIn1 $ concat $ tones0 ++ tones0 ++ tones1 ++ tones2 ++ tones2 ++ tones3 ++ tones3]
seq1 = [reverse $ concat [applyFadeIn fadeIn1 $ concat $ tones0 ++ tones0 ++ tones1 ++ tones2 ++ tones2 ++ tones3 ++ tones3]]

arptonesWav = genWaveFile $ seq0 ++ seq1
mnoisewav = genWaveFile $ mnoise0 ++ mnoise1 ++ mnoise0
ptonesWav = genWaveFile $ ptones0 ++ ptones1 ++ ptones0
stonesWav = genWaveFile $ stones0 ++ stones1 ++ stones0
dotonesWav = genWaveFile $ dotones0 ++ dotones1 ++ dotones0

rtonesWav = genWaveFile $ rtones
snWav = genWaveFile  [sn20, sn30, sn40, sn50, sn60, sn70] 
    
outputWav = rtonesWav
