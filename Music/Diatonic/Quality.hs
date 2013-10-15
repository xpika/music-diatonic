-- | The module implements musical qualities. Qualities are generic and are shared between various
-- types of musical objects.
module Music.Diatonic.Quality where


data Quality = Major | Minor | Neutral | Diminished | Augmented | Perfect 
               deriving (Eq, Show)


class Qual a where 
  -- | Return the quality of the 'Qual' instance.
  quality :: a -> Quality
