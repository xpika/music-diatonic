{-# LANGUAGE FlexibleInstances #-}

-- | The Note module implements the basic Music.Diatonic musical objects:
--
--   - Note
--
--   - Accidental

module Music.Diatonic.Note (
    Note(
        C,D,E,F,G,A,B
      ), Nte(..), Nts(..),
      next, prev, above, below, distance, raise, lower, sharp, flat, natural, accidental,
	canonize, circleOfFifths, transpose,
    Accidental(..)
  ) where


import Music.Diatonic.Interval 
import Music.Diatonic.Equivalence 



-- | Accidentals are rarely used directly. To alter 'Note's, use the 'raise' and 'lower' functions
--   (or their 'sharp' and 'flat' aliases).
data Accidental = Sharp | Flat | Natural
                  deriving (Eq, Ord)


instance Show Accidental where
  show Sharp = "#"
  show Flat = "b"
  show Natural = ""


instance Read Accidental where
  readsPrec _ ('#':cs) = [(Sharp, cs)]
  readsPrec _ ('b':cs) = [(Flat, cs)]
  readsPrec _ (cs) = [(Natural, cs)]



-- | Use these constructors to create 'Note's. To alter them, use the 'raise' and 'lower' functions
--   (or their 'sharp' and 'flat' aliases).
data Note = C | D | E | F | G | A | B | Note Accidental Note 
            deriving (Eq, Ord)


-- | Many musical objects have a note at their core (scales, chords, ...). The 'Nte' class allows these objects
--   to make use of all the note-manipulating functions.
class Nte a where
  -- | Applies a 'Note' manipulating function to an instance of the 'Nte' class.
  noteMap :: (Note -> Note) -> a -> a

  -- | Operator for 'noteMap'.
  ($#) :: (Note -> Note) -> a -> a
  ($#) = noteMap

  -- | Applies a 'Note' combining function to an instance of the 'Nte' class.
  notePlus :: (Note -> Note -> b) -> a -> a -> b


-- | Used to extract a list of notes from something (scale, chord, ...).
class Nts a where
  -- | Returns a list of 'Note's from a 'Nts' instance.
  notes :: a -> [Note]


instance Nte Note where
  noteMap f n = f n
  notePlus f n1 n2 = f n1 n2


instance Equiv Note where
  equiv n1 n2 = (snd . _distance n1 $ n2) == 0


instance Show Note where
  show C = "C" ; show D = "D" ; show E = "E" ; show F = "F" 
  show G = "G" ; show A = "A" ; show B = "B"
  show (Note Sharp n) = show n ++ show Sharp
  show (Note Flat n) = show n ++ show Flat


instance Read Note where
  readsPrec _ (l:as) | l `elem` "ABCDEFG" = acc (note l) as
    where note 'C' = C ; note 'D' = D ; note 'E' = E ; note 'F' = F 
          note 'G' = G ; note 'A' = A ; note 'B' = B 
          acc n ('s':cs) = acc n ('#':cs)
          acc n ('f':cs) = acc n ('b':cs)
          acc n as@('b':_) = [(foldl (\n' _ -> flat n') n k, d)]
            where (k, d) = break (/= 'b') as
          acc n as@('#':_) = [(foldl (\n' _ -> sharp n') n k, d)]
            where (k, d) = break (/= '#') as
          acc n cs = [(n, cs)]
  readsPrec _ _ | otherwise = []



-- | Raises a 'Note' by a semitone by applying an accidental. 
--   The note name stays the same.
raise :: Note -> Note
raise (Note Flat n) = n
raise n = Note Sharp n


-- | Alias for 'raise'.
sharp ::  Note -> Note
sharp = raise


-- | Lowers a 'Note' by a semitone by applying an accidental. 
--   The note name stays the same.
lower :: Note -> Note
lower (Note Sharp n) = n
lower n = Note Flat n


-- | Alias for 'lower'.
flat ::  Note -> Note
flat = lower


-- | Strips all 'Accidental's from a 'Note'.
natural :: Note -> Note
natural (Note a n) = natural n
natural n = n


-- | Return the 'Accidental' applied to the 'Note'.
accidental :: Note -> Accidental
accidental (Note a n) = a
accidental n = Natural


-- | Returns the next natural 'Note' in the cycle:
--   
-- @
--   C -> D -> E -> F -> G -> A -> B 
--   ^------------------------------
-- @
next :: Note -> Note
next C = D ; next D = E ; next E = F ; next F = G 
next G = A ; next A = B ; next B = C
next n = next . natural $ n


-- | Returns the previous natural 'Note' in the cycle:
--   
-- @
--   C -> B -> A -> G -> F -> E -> D 
--   ^------------------------------
-- @
prev :: Note -> Note
prev C = B ; prev D = C ; prev E = D ; prev F = E
prev G = F ; prev A = G ; prev B = A
prev n = prev . natural $ n


-- | Applies the specified 'Interval' upwards to a 'Note', returning the 'Note' above.
above :: Interval -> Note -> Note
above i n = adjust n' (semitones i - d)
  where n' = head . drop (steps i) . iterate next $ n
        d = snd . _distance n $ n'


-- | Applies the specified 'Interval' downwards to a 'Note', returning the 'Note' below.
below :: Interval -> Note -> Note
below i n = adjust n' (d - semitones i)
  where n' = head . drop (steps i) . iterate prev $ n
        d = snd . _distance n' $ n


-- | Returns the 'Interval' between the two 'Note's.
distance :: Note -> Note -> Interval
distance n1 n2 = head . drop (abs d) . iterate f $ base
  where 
    (stp, smt) = _distance n1 n2
    base = start stp
    start 0 = Unison ; start 1 = Maj2nd ; start 2 = Maj3rd ; start 3 = Perf4th
    start 4 = Perf5th ; start 5 = Maj6th ; start 6 = Maj7th
    d = smt - (semitones base)
    f = if d < 0 then diminish else augment


-- | Brings a 'Note' to it's most straight forward representation. For example:
--
-- > canonize (sharp B) == C
canonize :: Note -> Note 
canonize (Note Sharp B) = C
canonize (Note Sharp E) = F
canonize (Note Flat C) = B
canonize (Note Flat F) = E
canonize (Note Sharp (Note Sharp n)) = canonize $ Maj2nd `above` n
canonize (Note Flat (Note Flat n)) = canonize $ Maj2nd `below` n
canonize n = n


-- | Returns a list of 15 'Note's representing the circle of fifths centered around
--   the specified 'Note' (which is always the 7th element in the list). For example:
--
-- > circleOfFifths C = [Gb,Db,Ab,Eb,Bb,F,C,G,D,A,E,B,F#] 
circleOfFifths :: Note -> [Note]
circleOfFifths n = (reverse $ fifths below) ++ [n] ++ fifths above
  where fifths f = tail . take 7 . map canonize . iterate (f Perf5th) $ n


adjust :: Note -> Int -> Note
adjust n i = head . drop (abs i) . iterate f $ n
  where f  = if i < 0 then lower else raise


_distance :: Note -> Note -> (Int, Int)
_distance n1 n2 = (steps n1 n2, semis n1 n2)
  where
    steps n1 n2 | natural n1 == natural n2 = 0
    steps n1 n2 | otherwise = 1 + steps (next n1) n2

    semis C D = 2 ; semis D E = 2 ; semis E F = 1 ; semis F G = 2 ; semis G A = 2 
    semis A B = 2 ; semis B C = 1
    semis (Note Sharp n1) n2 = (semis n1 n2) - 1
    semis (Note Flat n1)  n2 = (semis n1 n2) + 1
    semis n1 (Note Sharp n2) = (semis n1 n2) + 1
    semis n1 (Note Flat n2) = (semis n1 n2) - 1
    semis n1 n2 | n1 == n2 = 0
    semis n1 n2 = (semis n1 n) + semis n n2
      where n = next n1


-- | Transposes instances of the 'Nte' class using the given 'Interval' and 
--   tranposition function. A typical use would be:
--
-- > transpose Min3rd above [D, sharp F, A] == [F,A,C]
--
transpose :: (Nte a) => Interval -> (Interval -> Note -> Note) -> a -> a 
transpose i f n = noteMap (f i) n
