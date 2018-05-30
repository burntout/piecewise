module Voices where
import Data.Int
import Data.WAVE
import System.Process
import System.Random
import VoiceWaves

data Voice = Voice { voiceName :: String, waveGen :: VoiceWave }
instance Show Voice where
    show (Voice name wavegen)  = show name
instance (Eq) Voice where
    (==) (Voice n0 w0) (Voice n1 w1) = n0 == n1

silence :: Voice
silence = Voice { voiceName = "silence", waveGen = genSilenceWave } 
sineWave :: Voice
sineWave = Voice { voiceName = "sineWave", waveGen = genSineWave }
organWave :: Voice
organWave = Voice { voiceName = "organWave", waveGen = genOrganWave }
wobbleWave :: Voice
wobbleWave = Voice { voiceName = "wobbleWave", waveGen = genSineWave }
squarePWWave :: Voice
squarePWWave = Voice { voiceName = "squarePWWave", waveGen = genPWSquareWave }
squareWave :: Voice
squareWave = Voice { voiceName = "squareWave", waveGen = genSquareWave }
saw1Wave :: Voice
saw1Wave = Voice { voiceName = "saw1Wave", waveGen = genSaw1Wave }
saw2Wave :: Voice
saw2Wave = Voice { voiceName = "saw2Wave", waveGen = genSaw2Wave }
saw3Wave :: Voice
saw3Wave = Voice { voiceName = "saw3Wave", waveGen = genSaw3Wave }
metalWave :: Voice
metalWave = Voice { voiceName = "metalWave", waveGen = genMetalNoise }
pluckWave :: Voice
pluckWave = Voice { voiceName = "pluckWave", waveGen = genPluckWave }
sn :: Voice
sn = Voice { voiceName = "sn", waveGen = genSn }
distSaw :: Voice
distSaw = Voice { voiceName = "distSaw", waveGen = genDistSaw }
distOrgan :: Voice
distOrgan = Voice { voiceName = "distOrgan", waveGen = genDistOrgan }
minorSquareStab :: Voice
minorSquareStab = Voice { voiceName = "minorSquareStab", waveGen = genMinorSquare }
minorSawStab :: Voice
minorSawStab = Voice { voiceName = "minorSawStab", waveGen = genMinorSaw2 }
