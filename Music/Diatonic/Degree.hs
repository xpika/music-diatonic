{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | This module implements scale degrees.

module Music.Diatonic.Degree (
    Degree(
      First,Second,Third,Fourth,Fifth,Sixth,Seventh
    ), 
    Deg(..), degree, from,
    second, third, fourth, fifth, sixth, seventh,
    tonic, supertonic, mediant, subdominant, dominant, submediant, subtonic, leadingTone
  ) where


import Music.Diatonic.Note
import Music.Diatonic.Interval
import Music.Diatonic.Equivalence
import Control.Monad (mplus)
import Data.List (find)



-- | Use these constructors to create 'Degree's. To alter them, use the 'flat' or 'sharp' functions along
-- with the '$#' operator.
data Degree = First | Second | Third | Fourth | Fifth | Sixth | Seventh | Degree Accidental Degree
              deriving (Eq, Ord)


instance Show Degree where
  show First = "1" ; show Second = "2" ; show Third = "3" ; show Fourth = "4" 
  show Fifth = "5" ; show Sixth = "6" ; show Seventh = "7" 
  show (Degree a d) = show a ++ show d


instance Nte Degree where
  noteMap f = fromNote . f . toNote
  notePlus f d1 d2 = f (toNote d1) (toNote d2)


instance Equiv Degree where 
  equiv d1 d2 = notePlus equiv d1 d2


class Eq n => Deg s n | s -> n where
  -- | Returns all the naturally occuring 'Degree's in s, along with the element that 
  -- corresponds to the 'Degree'.
  degrees :: s -> [(Degree, n)]

  -- | Returns the 'First' 'Degree' of s.
  first :: s -> n



-- | Assuming n1 as the tonic, returns the 'Degree' of n2.
degree :: Note -> Note -> Degree
degree n1 n2 = head . drop (abs d) . iterate f $ base
  where
    i = distance n1 $ n2
    (stp, smt) = (steps i, semitones i)
    base = start stp
    start 0 = First ; start 1 = Second ; start 2 = Third ; start 3 = Fourth
    start 4 = Fifth ; start 5 = Sixth ; start 6 = Seventh
    d = smt - (semitones . notePlus distance First $ base)
    f = if d < 0 then (lower $#) else (raise $#)


-- | Returns the 'Note' that corresponds to 'Degree' d in a scale where the specified 'Note' is the 'tonic'.
from :: Degree -> Note -> Note
from d n = above i n
  where i = notePlus distance First $ d


-- | Returns the 'Second' 'Degree' of s.
second :: Deg s n => s -> Maybe n
second = findVariant Second 


-- | Returns the 'Third' 'Degree' of s.
third :: Deg s n => s -> Maybe n
third = findVariant Third


-- | Returns the 'Fourth' 'Degree' of s.
fourth :: Deg s n => s -> Maybe n
fourth = findVariant Fourth


-- | Returns the 'Fifth' 'Degree' of s.
fifth :: Deg s n => s -> Maybe n
fifth = findVariant Fifth


-- | Returns the 'Sixth' 'Degree' of s.
sixth :: Deg s n => s -> Maybe n
sixth = findVariant Sixth


-- | Returns the 'Seventh' 'Degree' of s.
seventh :: Deg s n => s -> Maybe n
seventh = findVariant Seventh


-- | An alias for 'first'.
tonic :: Deg s n => s -> n
tonic = first


-- | An alias for 'second'.
supertonic :: Deg s n => s -> Maybe n
supertonic = second


-- | An alias for 'third'.
mediant :: Deg s n => s -> Maybe n
mediant = third


-- | An alias for 'fourth'.
subdominant :: Deg s n => s -> Maybe n
subdominant = fourth


-- | An alias for 'fifth'.
dominant :: Deg s n => s -> Maybe n
dominant = fifth


-- | An alias for 'sixth'.
submediant :: Deg s n => s -> Maybe n
submediant = sixth


-- | An alias for 'seventh', but only if the 'Seventh' is a 'Min7th' above the 'tonic'.
subtonic :: (Deg s n, Nte n) => s -> Maybe n
subtonic s = do
  let ton = tonic s
  svn <- seventh s
  if notePlus distance ton svn == Min7th 
    then return svn 
    else Nothing


-- | An alias for 'seventh', but only if the 'Seventh' is a 'Maj7th' above the 'tonic'.
leadingTone :: (Deg s n, Nte n) => s -> Maybe n
leadingTone s = seventh s >>= \svn -> (subtonic s `mplus` return svn)
  

findVariant :: Deg s n => Degree -> s -> Maybe n
findVariant d s = (find (\(d', n) -> natural $# d' == d) . degrees $ s) >>= return . snd


toNote :: Degree -> Note
toNote First   = C ; toNote Second = D ; toNote Third = E
toNote Fourth  = F ; toNote Fifth  = G ; toNote Sixth = A
toNote Seventh = B
toNote (Degree Sharp d) = sharp . toNote $ d
toNote (Degree Flat d)  = flat . toNote $ d


fromNote :: Note -> Degree
fromNote C = First  ; fromNote D = Second ; fromNote E = Third
fromNote F = Fourth ; fromNote G = Fifth  ; fromNote A = Sixth
fromNote B = Seventh
fromNote n | accidental n == Sharp = Degree Sharp . fromNote . lower $ n
fromNote n | accidental n == Flat  = Degree Flat . fromNote . raise $ n

