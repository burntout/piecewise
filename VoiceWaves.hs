module VoiceWaves where
import Data.Int
import Data.WAVE
import System.Process
import System.Random
import Pitches


-- type Effect = WaveCycle -> WaveCycle
type VoiceWave = Double -> Double -> [Double]
type VoiceWaveFormCycleGen = Pitch -> [Double -> Double] -> WaveCycle
type VoiceWaveFormGen = WaveCycle -> Float -> VoiceWave
type WaveCycle = [Double]

sampleRate = 44100::Int
bitrate = 32::Int

genWaveFormCycle :: VoiceWaveFormCycleGen
genWaveFormCycle p fs = samples 
    where 
        samples = fs <*> map (* (2 * p * pi / (fromIntegral sampleRate))) [0.0 .. (fromIntegral sampleRate ) / p]

-- genWaveForm :: VoiceWaveFormGen
genWaveForm wavecycle duration = take n $ concat $ repeat wavecycle
    where
        n = round (duration * (fromIntegral sampleRate))

-- genWaveFile :: [[Double]] -> WAVE
-- genWaveFile xs = WAVE header samples 
--     where 
--         header = WAVEHeader 1 sampleRate bitrate (Just $ length $ concat xs)
--         samples = xs >>= return . map doubleToSample 

-- silence / rest
genSilenceWaveCycle p = genWaveFormCycle 1 [\x -> 0]
genSilenceWave :: VoiceWave
genSilenceWave p d = genWaveForm (genSilenceWaveCycle p) d

-- Some sineWave related voices
genSineWaveCycle p = genWaveFormCycle p [\x -> 0.25 * sin(x)]

genSineWave :: VoiceWave
genSineWave p d = genWaveForm (genSineWaveCycle p) d

genOrganWaveCycle p = genWaveFormCycle p [\x -> 0.5 * (sin(x) + 0.5 * sin(2*x) + 0.25 * sin(4*x) + 0.125 * sin(8*x))]

genOrganWave :: VoiceWave
genOrganWave p d  = genWaveForm (genOrganWaveCycle p) d

wobbleFuncs = map (\x y -> 0.5 * sin (x * y) ) [1.0, 1.1 .. 3]
genWobbleWave :: VoiceWave 
genWobbleWave p d = genWaveForm (genWaveFormCycle p (wobbleFuncs ++ (reverse wobbleFuncs))) d

-- Some generalised square waves, with variable pulse width

squareFunc pw x 
    | x < 2 * pw * pi  = -0.3
    | x >= 2 * pw * pi = 0.3

sqfs = map ( squareFunc ) [0.10, 0.11 .. 0.90]

genPWSquareWaveCycle p = genWaveFormCycle p sqfs

genPWSquareWave :: VoiceWave
genPWSquareWave p d = genWaveForm (genPWSquareWaveCycle p) d 

genSquareWaveCycle p = genWaveFormCycle p [squareFunc 0.4]
genSquareWave :: VoiceWave
genSquareWave p d = genWaveForm (genSquareWaveCycle p) d 

-- Some generalised saw tooth waves
--
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


-- Some attempts to generate bursts of white noise.
--
genRandomCycle p = take (length [0.0 .. (fromIntegral sampleRate) / p]) $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
genMetalNoise :: VoiceWave
genMetalNoise p d  = genWaveForm (genRandomCycle p) d 


genWhiteNoise d =  take n $ randomRs (-0.5 :: Double, 0.5 :: Double) $ mkStdGen 42
    where n = round (d * (fromIntegral sampleRate))

-- Simple Karplus Strong Algorithm
-- http://crypto.stanford.edu/~blynn/sound/karplusstrong.html
ks xs = map (* 0.5) $ zipWith (+) xs (0:xs)
genPluckWave :: VoiceWave
genPluckWave p d = take n $ concat $ iterate ks $ randomDoubles p
    where 
        c = length [0.0 .. (fromIntegral sampleRate) / p]
        randomDoubles p  = take c $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
        n = round (d * (fromIntegral sampleRate))


-- Attempt to generate a tuned snare drum using a Karplus Strong technique
ksd xs = map (* 0.5) $ zipWith f xs (0:xs)
    where 
        f x y 
             | x >= y    = (x + y)
             | otherwise = 0
             -- | otherwise = -(x + y)

genSn :: VoiceWave
genSn p d  = take n $ concat $ iterate ksd $ randomDoubles p
    where 
        c = length [0.0 .. (fromIntegral sampleRate) / p]
        randomDoubles p  = take c $ randomRs (-0.5 :: Double, 0.5 :: Double) (mkStdGen 42)
        n = round (d * (fromIntegral sampleRate))

-- Normalize volumes
-- normalize :: Effect 
normalize xs = map (*scale) xs
   where
        scale = 0.5 / (maximum xs)

-- Simple Diode Distortion Filter
diodeFilter d xs = normalize $ diodeFilter' d xs
diodeFilter' _ [] = []
diodeFilter' d (w:ws)
    | (abs w) < d  = w:(diodeFilter' d ws)
    | otherwise    = ((signum  w) * d) : (diodeFilter' d ws)


-- Apply some distortion effects to waves
genDistSaw :: VoiceWave
genDistSaw p d  = diodeFilter 0.3 $ genSaw2Wave p d
genDistOrgan :: VoiceWave
genDistOrgan p d  = diodeFilter 0.20 $ genOrganWave p d
genDistSn p d = diodeFilter 0.20 $ genSn p d

-- Mixing waves ? 
mix wave1 wave2 
    | (length wave1) >= (length wave2) = zipWith (+) wave1 $ concat $ repeat wave2
    | otherwise                        = mix wave2 wave1

-- Generate Chords (it's like the dual of building arpeggios ? )
chord wave ps d = foldr mix [0] $ map (\p -> wave p d) ps 

-- generate some minor chord stab VoiceWaves
genMinorSquare :: VoiceWave
genMinorSquare p d  = chord genSquareWave ((map transpose [0, 3, 7]) <*> [p]) d
genMinorSaw2 :: VoiceWave
genMinorSaw2 p d  = chord genSaw2Wave ((map transpose [0, 3, 7]) <*> [p]) d
