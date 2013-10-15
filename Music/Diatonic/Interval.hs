-- |  The Interval module implements diatonic intervals.

module Music.Diatonic.Interval (
    Interval(
        Unison,Min2nd,Maj2nd,Min3rd,Maj3rd,Perf4th,
        Perf5th,Min6th,Maj6th,Min7th,Maj7th
      ), 
      compound, octave, min9th, maj9th, perf11th, min13th, maj13th,
      augment, diminish, 
      steps, semitones
  ) where


import Music.Diatonic.Quality
import Music.Diatonic.Equivalence



-- | Use these constructors to create 'Interval's. To alter them, use the 'diminish' or 'augment' functions.
data Interval = Unison | Min2nd | Maj2nd | Min3rd | Maj3rd | Perf4th
              | Perf5th | Min6th | Maj6th | Min7th | Maj7th 
              | Aug Interval | Dim Interval | Compound Interval
              deriving (Eq)


instance Qual Interval where
  quality i | i `elem` [Maj2nd, Maj3rd, Maj6th, Maj7th] = Major
  quality i | i `elem` [Min2nd, Min3rd, Min6th, Min7th] = Minor
  quality i | i `elem` [Unison, Perf4th, Perf5th] = Perfect
  quality (Aug i) = Augmented 
  quality (Dim i) = Diminished 
  quality (Compound i) = quality i 


instance Show Interval where
  show i = showq i ++ shown
    where shown = show . (+ 1) . steps $ i
          showq (Compound i) = showq i
          showq (Aug i@(Aug _)) = "A" ++ showq i
          showq (Dim i@(Dim _)) = "d" ++ showq i
          showq (Aug i) = "A" ++ (tail . showq $ i)
          showq (Dim i) = "d" ++ (tail . showq $ i)
          showq i = case quality i of
                      Major -> "M"
                      Minor -> "m"
                      Perfect -> "P"


instance Equiv Interval where 
  equiv i1 i2 = ((semitones i1 `mod` 12 == (semitones i2 `mod` 12)) && ((steps i1 `mod` 7) == (steps i2 `mod` 7)))



-- | Augments an 'Interval' by a semitone. The interval type remains the same.
augment :: Interval -> Interval
augment Min2nd = Maj2nd ; augment Min3rd = Maj3rd
augment Min6th = Maj6th ; augment Min7th = Maj7th
augment (Compound i) = compound . augment $ i
augment (Dim i) = i
augment i = Aug i


-- | Diminishes an 'Interval' by a semitone. The interval type remains the same.
diminish :: Interval -> Interval
diminish Maj2nd = Min2nd ; diminish Maj3rd = Min3rd
diminish Maj6th = Min6th ; diminish Maj7th = Min7th
diminish (Compound i) = compound . diminish $ i
diminish (Aug i) = i
diminish i = Dim i


-- | Returns the number of scale steps in an 'Interval'.
steps :: Interval -> Int
steps Unison  = 0 ;  steps Min2nd  = 1  ; steps Maj2nd  = 1   ; steps Min3rd = 2 
steps Maj3rd  = 2 ;  steps Perf4th = 3  ; steps Perf5th = 4   ; steps Min6th = 5 
steps Maj6th  = 5 ;  steps Min7th  = 6  ; steps Maj7th  = 6   ;
steps (Aug i) = steps i
steps (Dim i) = steps i
steps (Compound i) = 7 + steps i


-- | Returns the number of semitones in an 'Interval'.
semitones :: Interval -> Int
semitones Unison = 0   ; semitones Min2nd  = 1  ; semitones Maj2nd  = 2   ; semitones Min3rd = 3 
semitones Maj3rd = 4   ; semitones Perf4th = 5  ; semitones Perf5th = 7   ; semitones Min6th = 8 
semitones Maj6th = 9   ; semitones Min7th  = 10 ; semitones Maj7th  = 11  ;
semitones (Aug i) = semitones i + 1
semitones (Dim i) = semitones i - 1
semitones (Compound i) = 12 + semitones i


-- | Creates compound interval (adds an 'octave') to the specified 'Interval'
compound :: Interval -> Interval
compound = Compound 


octave :: Interval
octave = compound Unison


min9th :: Interval
min9th = compound Min2nd


maj9th :: Interval
maj9th = compound Maj2nd


perf11th :: Interval
perf11th = compound Perf4th


min13th :: Interval
min13th = compound Min6th


maj13th :: Interval
maj13th = compound Maj6th
