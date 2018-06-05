module Envelopes where

-- An envelope is just a list of doubles to scale the volume with

data Envelope = Envelope { name :: String, envl :: [Double] }
instance Show Envelope where
    show (Envelope name envl)  = show envl
instance (Eq) Envelope where
    (==) (Envelope n0 e0) (Envelope n1 e1) = n0 == n1


genEnvelope m a s d = attack ++ sustain ++ decay
    where
        attack = map (\x -> x/a) [0 .. a]
        sustain = take s $ repeat m
        decay = map (\x -> x/d) $ reverse [0 .. d]

applyEnvelope env wave = zipWith (*) wave $ env ++ (repeat 0)

-- genFadeIn t = map (\x -> x/t) [0 .. t]
-- applyFadeIn env wave = zipWith (*) wave $ env ++ (repeat 1) 

-- Some constant scalings
constEnv0 = Envelope { name = "constEnv0", envl = repeat 0.0 }
constEnv1 = Envelope { name = "constEnv1", envl = repeat 1.0 }
constEnv2 = Envelope { name = "constEnv2", envl = repeat 0.5 }
constEnv3 = Envelope { name = "constEnv3", envl = repeat 0.3 }
constEnv4 = Envelope { name = "constEnv4", envl = repeat 0.3 }

-- Some envelopes
bwap1Env = Envelope { name = "bwap1Env", envl = genEnvelope 1.0 4000 1000 4000 }
bwap2Env = Envelope { name = "bwap2Env", envl = genEnvelope 1.0 2200 500 2000 }
bwap3Env = Envelope { name = "bwap3Env", envl = genEnvelope 1.0 100 3000 2000 }
short1Env = Envelope { name = "short1Env", envl = genEnvelope 1.0 1000 10 1000 }

bwap1Env2 = Envelope { name = "bwap1Env2", envl = genEnvelope 0.5 4000 1000 4000 }
bwap2Env2 = Envelope { name = "bwap2Env2", envl = genEnvelope 0.5 2200 500 2000 }
bwap3Env2 = Envelope { name = "bwap3Env2", envl = genEnvelope 0.5 100 3000 2000 }
short1Env2 = Envelope { name = "short1Env2", envl = genEnvelope 0.5 1000 10 1000 }

bwap1Env3 = Envelope { name = "bwap1Env3", envl = genEnvelope 0.3 4000 1000 4000 }
bwap2Env3 = Envelope { name = "bwap2Env3", envl = genEnvelope 0.3 2200 500 2000 }
bwap3Env3 = Envelope { name = "bwap3Env3", envl = genEnvelope 0.3 100 3000 2000 }
short1Env3 = Envelope { name = "short1Env3", envl = genEnvelope 0.3 1000 10 1000 }
