{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module implements chords.

module Music.Diatonic.Chord (
    Chord, root,
    majorChord, minorChord, diminishedChord, augmentedChord,
    major7thChord, dominant7thChord, minor7thChord, minorMajor7thChord, minor7thFlat5thChord, 
      diminished7thChord, augmentedMajor7thChord
    --findChord
  ) where 
  


import Music.Diatonic
import Music.Diatonic.Scale
import Data.List (nub, sort, find) 



data Chord = Triad Quality Note
           | Tetrad Chord Interval
           deriving (Eq)


instance Nts Chord where
  notes c = n : (map (\i -> i `above` n) . intervals $ c)
    where n = root c
          intervals (Triad Major _) = [Maj3rd, Perf5th]
          intervals (Triad Minor _) = [Min3rd, Perf5th]
          intervals (Triad Diminished _) = [Min3rd, diminish Perf5th]
          intervals (Triad Augmented _) = [Maj3rd, augment Perf5th]
          intervals (Tetrad t i) = intervals t ++ [i]

  
instance Nte Chord where
  noteMap f (Triad q n) = Triad q $ f n
  noteMap f (Tetrad c i) = Tetrad (noteMap f c) i
  notePlus f c1 c2 = f (root c1) (root c2)



instance Deg Chord Note where
  first = root
  degrees c = map (\n -> (notePlus degree (root c) n, n)) ns
    where ns = notes c


instance Show Chord where
  show c = showDesc c
    where showDesc (Triad Major n) = show n
          showDesc (Triad Minor n) = show n ++ "m"
          showDesc (Triad Diminished n) = show n ++ "o"
          showDesc (Triad Augmented n) = show n ++ "+"
          showDesc (Tetrad t@(Triad Major n) Maj7th) = showDesc t ++ "maj7"
          showDesc (Tetrad t@(Triad Major n) Min7th) = showDesc t ++ "7"
          showDesc (Tetrad t@(Triad Minor n) Maj7th) = showDesc t ++ "maj7"
          showDesc (Tetrad t@(Triad Minor n) Min7th) = showDesc t ++ "7"
          showDesc (Tetrad t@(Triad Augmented n) Maj7th) = show n ++ "maj7" ++ "(#5)"
          showDesc (Tetrad (Triad Diminished n) Min7th) = show n ++ "m7" ++ "(b5)"
          showDesc (Tetrad (Triad Diminished n) i) | i == diminish Min7th = show n ++ "o7"


instance Read Chord where
  readsPrec x cs = 
    case readNote of
      []          -> []
      [(n, rest)] -> 
        case rest of
          ('m':'a':'j':'7':'(':'#':'5':')':cs) -> [(augmentedMajor7thChord n, cs)]
          ('m':'7':'(':'b':'5':')':cs)         -> [(minor7thFlat5thChord n, cs)]
          ('m':'7':'b':'5':cs)                 -> [(minor7thFlat5thChord n, cs)]
          ('m':'m':'a':'j':'7':cs)             -> [(minorMajor7thChord n, cs)]
          ('m':'a':'j':'7':cs)                 -> [(major7thChord n, cs)]
          ('m':'7':cs)                         -> [(minor7thChord n, cs)]
          ('o':'7':cs)                         -> [(diminished7thChord n, cs)]
          ('7':cs)                             -> [(dominant7thChord n, cs)]
          ('o':cs)                             -> [(diminishedChord n, cs)]
          ('+':cs)                             -> [(augmentedChord n, cs)]
          ('m':cs)                             -> [(minorChord n, cs)]
          cs                                   -> [(majorChord n, cs)]
    where readNote = readsPrec x cs 


instance Qual Chord where 
  quality (Triad q _) = q
  quality (Tetrad t _) = quality t


instance Equiv Chord where
  equiv c1 c2 = enote && etype
    where enote = notePlus equiv c1 c2
          etype = (toC $# c1) == (toC $# c2)
          toC = const C



-- | Returns the root of the 'Chord'.
root :: Chord -> Note
root (Triad q n) = n
root (Tetrad c i) = root c


-- | Builds a 'Major' 'Chord' (1-3-5) rooted at the specified 'Note'.
majorChord :: Note -> Chord
majorChord = Triad Major


-- | Builds a 'Minor' 'Chord' (1-b3-5) rooted at the specified 'Note'.
minorChord :: Note -> Chord
minorChord = Triad Minor


-- | Builds a 'Diminished' 'Chord' (1-b3-b5) rooted at the specified 'Note'.
diminishedChord :: Note -> Chord
diminishedChord = Triad Diminished


-- | Builds a 'Diminished' 'Chord' (1-3-#5) rooted at the specified 'Note'.
augmentedChord :: Note -> Chord
augmentedChord = Triad Augmented


-- | Builds a 'Major' 7th 'Chord' (1-3-5-7) rooted at the specified 'Note'.
major7thChord :: Note -> Chord
major7thChord n = Tetrad (majorChord n) Maj7th


-- | Builds a Dominant 7th 'Chord' (1-3-5-b7) rooted at the specified 'Note'.
dominant7thChord :: Note -> Chord
dominant7thChord n = Tetrad (majorChord n) Min7th


-- | Builds a 'Minor' 7th 'Chord' (1-b3-5-b7) rooted at the specified 'Note'.
minor7thChord :: Note -> Chord
minor7thChord n = Tetrad (minorChord n) Min7th


-- | Builds a 'Minor' Major 7th 'Chord' (1-b3-5-7) rooted at the specified 'Note'.
minorMajor7thChord :: Note -> Chord
minorMajor7thChord n = Tetrad (minorChord n) Maj7th


-- | Builds a 'Diminished' 7th 'Chord' (1-b3-b5-bb7) rooted at the specified 'Note'.
diminished7thChord :: Note -> Chord
diminished7thChord n = Tetrad (diminishedChord n) $ diminish Min7th


-- | Builds an 'Augmented' Major 7th 'Chord' (1-3-#5-7) rooted at the specified 'Note'.
augmentedMajor7thChord :: Note -> Chord
augmentedMajor7thChord n = Tetrad (augmentedChord n) Maj7th


-- | Builds a 'Minor' Flat 5th 'Chord' (1-b3-b5-b7) rooted at the specified 'Note'.
minor7thFlat5thChord :: Note -> Chord
minor7thFlat5thChord n = Tetrad (diminishedChord n) Min7th



chordTypes = [majorChord, minorChord, diminishedChord, augmentedChord, 
              major7thChord, dominant7thChord, minor7thChord, minorMajor7thChord, minor7thFlat5thChord, diminished7thChord, augmentedMajor7thChord]


-- Given the root and a list of other notes, determines 
-- the Chord.
findChord :: Note -> [Note] -> Maybe Chord
findChord r ns = find (\c -> (uns . notes $ c) == (uns $ r:ns)) . map ($ r) $ chordTypes
  where uns ns = sort . map show . nub $ ns
