module Notes where

import qualified Pitches as P
import Voices

type Pattern = [Note]
type Pitch = Double
data Note = Note { pitch :: (Maybe Pitch), voice :: (Maybe Voice), duration :: (Maybe Float) } deriving (Show)

rest = Note { pitch = 1, voice = silence, duration = Nothing } 
a4 = Note { pitch = (Just P.a4), voice = Nothing, duration = Nothing }

noteTranspose :: Double -> Note -> Note
noteTranspose i n = Note {pitch = p, voice = (voice n), duration = (duration n) }
    where 
       p = pitch n >>= return . P.transpose i
