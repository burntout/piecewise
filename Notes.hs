module Notes where

import qualified Pitches as P
import Voices

type Pattern = [Note]
type Pitch = Double
data Note = Note { pitch :: (Maybe Pitch), voice :: (Maybe Voice), duration :: (Maybe Float) } deriving (Show)

rest = Note { pitch = (Just 1), voice = (Just silence), duration = Nothing } 
a1 = Note { pitch = (Just P.a1), voice = Nothing, duration = Nothing }
a2 = Note { pitch = (Just P.a2), voice = Nothing, duration = Nothing }
a3 = Note { pitch = (Just P.a3), voice = Nothing, duration = Nothing }
a4 = Note { pitch = (Just P.a4), voice = Nothing, duration = Nothing }

noteTranspose :: Double -> Note -> Note
noteTranspose i n = Note {pitch = p, voice = (voice n), duration = (duration n) }
    where 
       p = pitch n >>= return . P.transpose i

setVoice v n = Note { pitch = (pitch n), voice = (Just v), duration = (duration n) }
setDuration d n =  Note { pitch = (pitch n), voice = (voice n), duration = Just d } 

arp1arp = map noteTranspose [0, 5, 7, -5, 0, 5, 7, -2]
arp0 = arp1arp <*> [a1, a1, a2, a2]
arp1 = arp1arp <*> (arp1arp <*> [a1, a1, a2, a2])

voiceMap = map setVoice [squareWave, squareWave, silence, squarePWWave, squarePWWave ]
p0 = map (setDuration 0.125) (zipWith ($) (concat $ repeat voiceMap) arp0)
p1 = map (setDuration 0.125) (zipWith ($) (concat $ repeat voiceMap) arp1)

--patternToDoubles [] = []
--patternToDoubles ns
--    where
--        ds = filter (duration n /= Nothing) ns
--        d = head ds
--        
        
