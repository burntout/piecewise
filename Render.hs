module Render where
import Data.Int
import Data.WAVE
import Patterns
import Pitches
import System.Process
import System.Random
import Synths


main :: IO ()
main = 
    do 
        -- putWAVEFile "out.wav" $ genWaveFile [whiteNoiseWaves]
        putWAVEFile "out.wav" outputWav
        pid <- runCommand "aplay -q out.wav"
        -- waitForProcess pid
        return ()
