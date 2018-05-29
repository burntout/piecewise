module Synths where
import Data.Int
import Data.WAVE
import System.Process
import System.Random
import Pitches

type SynthWave = Double -> Float -> [Double]
data Synth = Synth { synthName :: String, waveGen :: SynthWave }
instance Show Synth where
    show (Synth name wavegen)  = show name

sampleRate = 44100
bitrate = 32

genWaveFormCycle p fs = samples 
    where 
        samples = fs <*> map (* (2 * p * pi / (fromIntegral sampleRate))) [0.0 .. (fromIntegral sampleRate ) / p]

genWaveForm wavecycle duration = take n $ concat $ repeat wavecycle
    where
        n = round (duration * (fromIntegral sampleRate))

genWaveFile xs = WAVE header samples 
    where 
        header = WAVEHeader 1 sampleRate bitrate (Just $ length $ concat xs)
        samples = xs >>= return . map doubleToSample 

genSineWaveCycle p = genWaveFormCycle p [\x -> 0.25 * sin(x)]

genSineWave :: SynthWave
genSineWave p d = genWaveForm (genSineWaveCycle p) d

genOrganWaveCycle p = genWaveFormCycle p [\x -> 0.5 * (sin(x) + 0.5 * sin(2*x) + 0.25 * sin(4*x) + 0.125 * sin(8*x))]
genOrganWave p d  = genWaveForm (genOrganWaveCycle p) d

wobbleFuncs = map (\x y -> 0.5 * sin (x * y) ) [1.0, 1.1 .. 3]
genWobbleWave p d = genWaveForm (genWaveFormCycle p (wobbleFuncs ++ (reverse wobbleFuncs))) d
wobbleWave = genWaveFile $ concat  $ replicate 4  [ genWobbleWave 55 3.0 , genWobbleWave 97.99886 1.0]

squareFunc pw x 
    | x < 2 * pw * pi  = -0.5
    | x >= 2 * pw * pi = 0.5

sqfs = map ( squareFunc ) [0.10, 0.11 .. 0.90]

genPWSquareWaveCycle p = genWaveFormCycle p sqfs
genPWSquareWave p d = genWaveForm (genPWSquareWaveCycle p) d 

genSquareWaveCycle p = genWaveFormCycle p [squareFunc 0.4]
genSquareWave p d = genWaveForm (genSquareWaveCycle p) d 

saw1 x = -0.5 + x/(2 * pi)

saw2 d x
    | x < d                         = -0.5 * x / d
    | (x >=d) && (x < (2 * pi - d)) = (x - pi) / (2 * (pi -d))
    | x >= 2 * pi - d               = (-0.5 * x + pi) / d

saws = map saw2 $ [0.1, 0.2 .. 1.5] ++ (reverse [0.1, 0.2 .. 1.5])

genSaw1WaveCycle p = genWaveFormCycle p [saw1]
genSaw1Wave p d = genWaveForm (genSaw1WaveCycle p) d 

genSaw2WaveCycle p = genWaveFormCycle p [saw2 (pi/2)]
genSaw2Wave p d = genWaveForm (genSaw2WaveCycle p) d 

genSaw3WaveCycle p = genWaveFormCycle p saws
genSaw3Wave p d = genWaveForm (genSaw3WaveCycle p) d 


genRandomCycle p = take (length [0.0 .. (fromIntegral sampleRate) / p]) $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
genMetalNoise p d  = genWaveForm (genRandomCycle p) d 

genWhiteNoise d =  take n $ randomRs (-0.5 :: Double, 0.5 :: Double) $ mkStdGen 42
    where n = round (d * (fromIntegral sampleRate))

-- Simple Karplus Strong Algorithm
-- http://crypto.stanford.edu/~blynn/sound/karplusstrong.html
ks xs = map (* 0.5) $ zipWith (+) xs (0:xs)
genPluckWave p d = take n $ concat $ iterate ks $ randomDoubles p
    where 
        c = length [0.0 .. (fromIntegral sampleRate) / p]
        randomDoubles p  = take c $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
        n = round (d * (fromIntegral sampleRate))

ksd xs = map (* 0.5) $ zipWith f xs (0:xs)
    where 
        f x y 
             | x >= y    = (x + y)
             | otherwise = 0
             -- | otherwise = -(x + y)

genSn p d  = take n $ concat $ iterate ksd $ randomDoubles p
    where 
        c = length [0.0 .. (fromIntegral sampleRate) / p]
        randomDoubles p  = take c $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
        n = round (d * (fromIntegral sampleRate))

normalize xs = map (*scale) xs
   where
        scale = 0.5 / (maximum xs)

-- Simple Diode Filter
diodeFilter d xs = normalize $ diodeFilter' d xs
diodeFilter' _ [] = []
diodeFilter' d (w:ws)
    | (abs w) < d  = w:(diodeFilter' d ws)
    | otherwise    = ((signum  w) * d) : (diodeFilter' d ws)

genDistSaw p d  = diodeFilter 0.3 $ genSaw2Wave p d
genDistOrgan p d  = diodeFilter 0.20 $ genOrganWave p d


genEnvelope a s r = attack ++ sustain ++ release
    where
        attack = map (\x -> x/a) [0 .. a]
        sustain = take s $ repeat 1.0
        release = map (\x -> x/r) $ reverse [0 .. r]

genFadeIn t = map (\x -> x/t) [0 .. t]
applyEnvelope env wave = zipWith (*) wave $ env ++ (repeat 0)
applyFadeIn env wave = zipWith (*) wave $ env ++ (repeat 1) 

env1 = genEnvelope 4000 1000 4000
env2 = genEnvelope 1000 10 1000
env3 = genEnvelope 2200 500 2000
env4 = genEnvelope 100 3000 2000
fadeIn1 = genFadeIn 200000

--
-- doesn't work because need to convert to double ? 
mix wave1 wave2 
    | (length wave1) >= (length wave2) = zipWith (+) wave1 $ concat $ repeat wave2
    | otherwise                        = mix wave2 wave1

chord wave ps d = foldr mix [0] $ map (\p -> wave p d) ps 
chordf ws ps d = foldr mix [0] $ ws <*> ps 

genMinorSquare p d  = chord genSquareWave ((map transpose [0, 3, 7]) <*> [p]) d
genMinorSaw2 p d  = chord genSaw2Wave ((map transpose [0, 3, 7]) <*> [p]) d

-- squareWaves  = genWaveFile $ concat $ replicate 4 $ map (applyEnvelope env1) [genSquareWave 440 0.5, genSquareWave 220 0.5]
-- organWaves  = genWaveFile $ concat $ replicate 4 $ map (applyEnvelope env1) [genOrganWave 440 1.5, genOrganWave 55 1.5]
-- whiteNoiseWaves = genWhiteNoise 1.5
-- 
