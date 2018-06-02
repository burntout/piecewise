module Notes where

import qualified Durations as D
import Envelopes
import qualified Pitches as P
import Voices
import VoiceWaves
import Data.WAVE
import Data.Int
import Data.Maybe
import System.Process

type Pattern = [Note]
type Pitch = Double
data Note = Note { pitch :: (Maybe Pitch), voice :: (Maybe Voice), duration :: (Maybe Double), envelope :: (Maybe Envelope) } deriving (Show)

-- sampleRate = 44100
-- bitrate = 32

rest = Note { pitch = (Just 1), voice = (Just silence), duration = Nothing, envelope = Nothing } 
a1 = Note { pitch = (Just P.a1), voice = Nothing, duration = Nothing, envelope = Nothing }
e1 = Note { pitch = (Just P.e1), voice = Nothing, duration = Nothing, envelope = Nothing }
f1 = Note { pitch = (Just P.f1), voice = Nothing, duration = Nothing, envelope = Nothing }
a2 = Note { pitch = (Just P.a2), voice = Nothing, duration = Nothing, envelope = Nothing }
a3 = Note { pitch = (Just P.a3), voice = Nothing, duration = Nothing, envelope = Nothing }
a4 = Note { pitch = (Just P.a4), voice = Nothing, duration = Nothing, envelope = Nothing }

noteTranspose :: Double -> Note -> Note
noteTranspose i n = Note {pitch = p, voice = (voice n), duration = (duration n), envelope = (envelope n) }
    where 
        p = P.transpose i <$> pitch n
        -- p = pitch n >>= return . P.transpose i

setVoice v n = Note { pitch = (pitch n), voice = (Just v), duration = (duration n), envelope = (envelope n) }
setDuration d n =  Note { pitch = (pitch n), voice = (voice n), duration = Just d, envelope = (envelope n) } 
setEnvelope e n = Note { pitch = (pitch n), voice = (voice n), duration = (duration n), envelope = Just e } 

arp1arp = map noteTranspose $  [0, 0, 7, 10, 0, 0, 0, -2] ++ [0, -2, 7, 9, 0, 0, -2, -4]
arp0 = arp1arp <*> [a2, a2, a3, a3]
arp1 = arp1arp <*> arp0

foob = concat $ replicate 2 [e1, a4]
foobVoice = map setVoice [wobbleWave, sn]
foobEnv = map setEnvelope [ bwap3Env, constEnv1]
foobDur = map setDuration [D.n4, D.n4]

fooba = concat $ replicate 8 [a1, a4, a4, a4, f1, a2, a3]
foobaVoice = map setVoice [ organWave, sn, sn, distSn, distSn, distSn, distSn]
foobaDur = map setDuration [ D.n4, D.n16, D.n16 ,D.n8, D.n4, D.n8d, D.n16]

v0 = zipWith ($) (concat $ repeat foobVoice) $ foob
d0 = zipWith ($) (concat $ repeat foobDur) v0
p0 = zipWith ($) (concat $ repeat foobEnv) d0

v1 = zipWith ($) (concat $ repeat foobaVoice) $ fooba
d1 = zipWith ($) (concat $ repeat foobaDur) v1
p1 = zipWith ($) (concat $ repeat foobEnv) d1


voiceMap = map setVoice [saw1Wave, saw2Wave, saw3Wave, silence, squareWave, squarePWWave, silence]
-- voiceMap = map setVoice [organWave, sineWave, organWave, silence, sineWave, distOrgan, silence]
-- voiceMap = map setVoice [organWave, distOrgan, silence, distOrgan, organWave]

envMap = map setEnvelope [bwap3Env, constEnv1, constEnv2, short1Env]

-- rhythmMap = map setDuration [D.n16, D.n16, D.n8]
rhythmMap = map setDuration [D.n8, D.n16, D.n16, D.n8, D.n16, D.n16]

v2 = zipWith ($) (concat $ repeat voiceMap) arp0
v3 = zipWith ($) (concat $ repeat voiceMap) arp1

d3 = zipWith ($) (concat $ repeat rhythmMap) v3

p2 = zipWith ($) (concat $ repeat envMap) d3
p3 = zipWith ($) (concat $ repeat envMap) d3


outPattern = [p0, p0, p1, p0, p0]

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


-- Take a two  Patterns .. and mux them together
-- If they are different lengths repeat the shorter
-- to the length of the longer
-- stack2 :: Pattern -> Pattern -> Maybe [[Double]]
stack2 p0 p1 = 
    do { ds0 <- patternToDoubles p0;
        do { ds1 <- patternToDoubles p1;
           return (genWaveFile [(normalize ( mix (concat ds0) (concat ds1)))]) }}
 
 
render :: [Pattern] ->  Maybe WAVE
render ps = 
    do { ds <- patternToDoubles $ concat ps;
        return $ genWaveFile ds 
    }

main :: IO ()
main =
    do
        -- let outWav = fromJust $ render outPattern
        let outWav = fromJust $ stack2 p3 p1
        putWAVEFile "out.wav" outWav 
        pid <- runCommand "aplay -q out.wav"
        return ()
