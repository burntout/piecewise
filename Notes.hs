module Notes where

import qualified Durations as D
import Envelopes
import qualified Pitches as P
import Voices
import Data.WAVE
import Data.Int
import Data.Maybe
import System.Process

type Pattern = [Note]
type Pitch = Double
data Note = Note { pitch :: (Maybe Pitch), voice :: (Maybe Voice), duration :: (Maybe Double), envelope :: (Maybe Envelope) } deriving (Show)

sampleRate = 44100
bitrate = 32

rest = Note { pitch = (Just 1), voice = (Just silence), duration = Nothing, envelope = Just constEnv1 } 
a1 = Note { pitch = (Just P.a1), voice = Nothing, duration = Nothing, envelope = Just constEnv1 }
a2 = Note { pitch = (Just P.a2), voice = Nothing, duration = Nothing, envelope = Just constEnv1 }
a3 = Note { pitch = (Just P.a3), voice = Nothing, duration = Nothing, envelope = Just constEnv1 }
a4 = Note { pitch = (Just P.a4), voice = Nothing, duration = Nothing, envelope = Just constEnv1 }

noteTranspose :: Double -> Note -> Note
noteTranspose i n = Note {pitch = p, voice = (voice n), duration = (duration n), envelope = (envelope n) }
    where 
        p = P.transpose i <$> pitch n
        -- p = pitch n >>= return . P.transpose i

setVoice v n = Note { pitch = (pitch n), voice = (Just v), duration = (duration n), envelope = (envelope n) }
setDuration d n =  Note { pitch = (pitch n), voice = (voice n), duration = Just d, envelope = (envelope n) } 
setEnvelope e n = Note { pitch = (pitch n), voice = (voice n), duration = (duration n), envelope = Just e } 

arp1arp = map noteTranspose $  [0, 0, 7, 10, 0, 0, 0, -2] ++ [0, -2, 7, 9, 0, 0, -2, -4]
-- arp1arp = map noteTranspose $  [0, 0, 10, 8, 5, -4, -2 , 1]
arp0 = arp1arp <*> [a2, a2, a3, a3]
arp1 = arp1arp <*> arp0

-- voiceMap = map setVoice [saw1Wave, saw2Wave, saw3Wave, silence, squareWave, squarePWWave, silence]
-- voiceMap = map setVoice [organWave, sineWave, organWave, silence, sineWave, distOrgan, silence]
voiceMap = map setVoice [organWave, distOrgan, silence, distOrgan, organWave]

envMap = map setEnvelope [bwap3Env, constEnv1, constEnv2, short1Env]

-- rhythmMap = map setDuration [D.n16, D.n16, D.n8]
rhythmMap = map setDuration [D.n16, D.n16, D.n8, D.n16, D.n16]

v0 = zipWith ($) (concat $ repeat voiceMap) arp0
v1 = zipWith ($) (concat $ repeat voiceMap) arp1

d0 = zipWith ($) (concat $ repeat rhythmMap) v0
d1 = zipWith ($) (concat $ repeat rhythmMap) v1

p0 = zipWith ($) (concat $ repeat envMap) d0
p1 = zipWith ($) (concat $ repeat envMap) d1


outPattern = [p0, p1]

noteToDoubles :: Note -> Maybe [Double]
noteToDoubles n =  
    do { d <- duration n ;
        do { p <- pitch n ;
            do { v <- voice n ;
                do { e <- envelope n;
                return $ applyEnvelope (envl e) ((waveGen v) p d) }}}}

noteToSample :: Note -> Maybe [Int32]
noteToSample n = map doubleToSample <$> (noteToDoubles n)

patternToDoubles :: Pattern -> Maybe [[Double]]
patternToDoubles p = sequence $ map noteToDoubles p 
patternToSample :: Pattern  -> Maybe [[Int32]]
patternToSample p = sequence $ map noteToSample p 

genWaveFile :: [[Double]] -> WAVE
genWaveFile xs = WAVE header samples
    where
        header = WAVEHeader 1 sampleRate bitrate (Just $ length $ concat xs)
        samples = xs >>= return . map doubleToSample

render :: [Pattern] ->  Maybe WAVE
render ps = 
    do { ds <- patternToDoubles $ concat ps;
        return $ genWaveFile ds 
    }

main :: IO ()
main =
    do
        let outWav = fromJust $ render outPattern
        putWAVEFile "out.wav" outWav 
        pid <- runCommand "aplay -q out.wav"
        return ()
