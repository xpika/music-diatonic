{-# LANGUAGE MultiParamTypeClasses #-}

-- | The Scale module implements scales.
module Music.Diatonic.Scale (
  Scale, Scl(..),
    majorScale, minorScale, majorPentatonicScale, minorPentatonicScale, minorHarmonicScale, minorMelodicScale,
    tetrachord
  ) where 
 

import Music.Diatonic


data Scale = Diatonic Quality Note 
           | Pentatonic Quality Note
           | Harmonic Note
           | Melodic Note
             deriving (Eq)


class Scl a where 
  scale :: a -> Scale


instance Nte Scale where
  noteMap f (Diatonic Major n) = majorScale . f $ n
  noteMap f (Diatonic Minor n) = minorScale . f $ n
  noteMap f (Pentatonic Major n) = majorPentatonicScale . f $ n
  noteMap f (Pentatonic Minor n) = minorPentatonicScale . f $ n
  noteMap f (Harmonic n) = minorHarmonicScale . f $ n
  noteMap f (Melodic n) = minorMelodicScale . f $ n
  notePlus f s1 s2 = f (tonic s1) (tonic s2)


instance Nts Scale where
  notes (Diatonic Major n) = init $ tc1 ++ tc2
    where tc1 = tetrachord n
          tc2 = tetrachord (Maj2nd `above` last tc1)
  notes (Diatonic Minor n) = zipWith ($) [id, id, lower, id, id, lower, lower] (notes . majorScale $ n)
  notes (Pentatonic Major n) = concat . zipWith ($) [return, return, return, const [], return, return, const []] $ (notes . majorScale $ n)
  notes (Pentatonic Minor n) = concat . zipWith ($) [return, const [], return, return, return, const [], return] $ (notes . minorScale $ n)
  notes (Harmonic n) = zipWith ($) [id, id, id, id, id, id, raise] $ (notes . minorScale $ n)
  notes (Melodic n) = zipWith ($) [id, id, id, id, id, raise, raise] $ (notes . minorScale $ n)


instance Qual Scale where
  quality (Diatonic q _) = q
  quality (Pentatonic q _) = q
  quality (Harmonic _) = Minor
  quality (Melodic _) = Minor


instance Show Scale where
  show s@(Harmonic n) = (show n) ++ "m (harmonic)"
  show s@(Melodic n) = (show n) ++ "m (melodic)"
  show s@(Pentatonic q n) = (show n) ++ (if quality s == Minor then "m" else "") ++ " (pentatonic)"
  show s@(Diatonic q n) = (show n) ++ (if quality s == Minor then "m" else "")
 

instance Deg Scale Note where
  first (Diatonic q t) = t
  first (Pentatonic q t) = t
  first (Harmonic t) = t
  first (Melodic t) = t

  degrees s = map (\n -> (notePlus degree (first s) n, n)) ns
    where ns = notes s


instance Equiv Scale where 
  equiv s1 s2 = enote && etype
    where enote = notePlus equiv s1 s2
          etype = (toC $# s1) == (toC $# s2)
          toC = const C


-- | Creates a 'Major' diatonic 'Scale' using the given 'Note' as the tonic.
majorScale :: Note -> Scale
majorScale = Diatonic Major 


-- | Creates a 'Minor' diatonic 'Scale' using the given 'Note' as the tonic.
minorScale :: Note -> Scale
minorScale = Diatonic Minor


-- | Creates a 'Major' pentatonic 'Scale' using the given 'Note' as the tonic.
majorPentatonicScale :: Note -> Scale
majorPentatonicScale = Pentatonic Major


-- | Creates a 'Minor' pentatonic 'Scale' using the given 'Note' as the tonic.
minorPentatonicScale :: Note -> Scale
minorPentatonicScale = Pentatonic Minor


-- | Creates a 'Minor' harmonic 'Scale' using the given 'Note' as the tonic.
minorHarmonicScale :: Note -> Scale
minorHarmonicScale = Harmonic


-- | Creates a 'Minor' melodic 'Scale' using the given 'Note' as the tonic.
minorMelodicScale :: Note -> Scale
minorMelodicScale = Melodic


-- | Returns a tetrachord using the given 'Note' as the starting note.
--
-- > tetrachord G == [G,A,B,C]
tetrachord :: Note -> [Note]
tetrachord n = scanl (\n i -> i `above` n) n [Maj2nd, Maj2nd, Min2nd]
