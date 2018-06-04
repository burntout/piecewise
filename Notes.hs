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

note1Map = map noteTranspose [0, 0, 10]
notes1  = note1Map <*> [a1, e1]
durMap1 = map setDuration [D.n4d, D.n4d, D.n4]

d1 = zipWith ($) (concat $ repeat durMap1) notes1
v1 = map (setVoice saw3Wave) d1
p1 = concat $ replicate 8 $ map (setEnvelope bwap1Env) v1


notes2 = [a4,a3,a2,a1,a2,a3,a4]
durMap2 = map setDuration $ [D.n16, D.n16 ,D.n8] ++ (concat  $ replicate 4 [D.n16])
-- durMap2 = map setDuration $ (concat  $ replicate 8 [D.n16])
d2 = zipWith ($) durMap2 (concat $ repeat notes2)
v2 = map (setVoice distSn) d2
e2 = map setEnvelope [constEnv2, constEnv3, constEnv2]
-- p2 = concat $ replicate  16 $  map (setEnvelope constEnv1) v2
p2 = concat $ replicate  16 $  zipWith ($) (concat $ repeat e2) v2


note3Map = map noteTranspose [0,0,0,5,5,5]
notes3 = note3Map <*> [a4]
durMap3 = map setDuration $ [D.n16, D.n16, D.n8]
voiceMap3 = map setVoice $ (replicate 4 silence ) ++ (replicate 3 minorSquareStab) ++ (replicate 4 silence)
e3 = map setEnvelope [short1Env, bwap2Env, constEnv0, short1Env]

d3 = zipWith ($) (concat $ repeat durMap3) notes3
v3 = (voiceMap3) <*> d3
p3 = zipWith ($) (concat $ repeat e3) v3


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

-- Take a list of patterns, mux them together, return a Maybe [Double] that we can render
-- If they are different lengths repeat the shorter
-- to the length of the longer
--
stack' :: Maybe [Double] -> Pattern -> Maybe [Double]
stack' d p = 
    do { ds0 <- d;
        do { ds1 <- patternToDoubles p;
            return $ mix ds0 (concat ds1) }} 

stack :: [Pattern] -> Maybe [Double]
stack ps = (foldl stack' (Just [0.0]) ps) >>= return . normalize 

chain :: [Pattern] -> Maybe [Double]
chain ps = 
    do { ds <- patternToDoubles $ concat ps;
        return $ concat $ ds }

render :: Maybe [Double] ->  Maybe WAVE
render ds = 
    do { d0 <- ds;
        return $ genWaveFile [d0]
    }

main :: IO ()
main =
    do
        -- let outWav = fromJust $ render outPattern
        let outWav = genWaveFile [fromJust $ stack [p1, p2], fromJust $ stack [p1, p2,p1, p3], fromJust $ stack [p1, p2]]
        putWAVEFile "out.wav" outWav 
        -- pid <- runCommand "aplay -q out.wav"
        return ()
