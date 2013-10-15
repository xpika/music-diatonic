-- | This module inplements keys.

module Music.Diatonic.Key (
  Key,
    majorKey, minorKey, center, key,
    parallel, relative, relativeMajor, relativeMinor, 
  Signature,
    signature, sigDetails
  ) where 
 


import Music.Diatonic
import Music.Diatonic.Scale
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)


data Key = Key Quality Note
           deriving (Eq)


data Signature = Signature Key
                 deriving (Eq)


instance Nte Key where
  noteMap f (Key q n) = Key q $ f n
  notePlus f k1 k2 = f (center k1) (center k2)


instance Nts Signature where
  notes (Signature k@(Key Minor n)) = notes . signature . majorKey . center . relativeMajor $ k
  notes (Signature k) = by7s . sig $ k
    where sig (Key Major C) = []
          sig k = let n = center k 
                      usesb = any (\n -> accidental n == Flat) . notes . scale $ k in
                  if usesb
                    then (sig . majorKey $ (Perf5th `above` n)) ++ [Perf4th `above` n]
                    else (sig . majorKey $ (Perf5th `below` n)) ++ [Min2nd `below` n]
          by7s xs | length xs < 8 = xs
          by7s xs = map snd . sortBy (compare `on` fst) .  drop (length xs - 7) . 
                      flip zip xs . map (`mod` 7) $ [0 ..]


instance Show Key where
  show (Key Major n) = show n
  show (Key Minor n) = show n ++ "m"


instance Show Signature where
  show s = if null . notes $ s
             then ""
             else (show . notes $ s) ++ " (" ++ show n ++ " " ++ show a ++ ")"
    where (a, n) = sigDetails s


instance Read Key where
  readsPrec x cs =
    case readNote of
      []          -> []
      [(n, rest)] ->
        case rest of
          ('m':cs)  -> [(minorKey n, cs)]
          cs        -> [(majorKey n, rest)]
    where readNote = readsPrec x cs


instance Scl Key where 
  scale (Key Major n) = majorScale n
  scale (Key Minor n) = minorScale n


instance Qual Key where 
  quality (Key q n) = q


instance Equiv Key where
  equiv k1 k2 = enote && etype
    where enote = notePlus equiv k1 k2
          etype = (toC $# k1) == (toC $# k2)
          toC = const C



-- | Returns the 'Note' that is the key's center.
center :: Key -> Note
center (Key q n) = n


-- | Creates a 'Major' 'Key' centered around the specified 'Note'.
majorKey :: Note -> Key
majorKey = Key Major 


-- | Creates a 'Major' 'Key' centered around the specified 'Note'.
minorKey :: Note -> Key
minorKey = Key Minor 


-- | Creates a 'Key' based on the specified 'Scale'.
key :: Scale -> Key
key s | quality s == Major = majorKey . tonic $ s
key s | quality s == Minor = minorKey . tonic $ s


-- | Returns the parallel 'Key' of the specified 'Key'.
parallel :: Key -> Key
parallel (Key Major n) = minorKey n
parallel (Key Minor n) = majorKey n


-- | Returns the relative 'Key' of the specified 'Key'.
relative :: Key -> Key
relative k@(Key Minor n) = majorKey . center . relativeMajor $ k
relative k@(Key Major n) = minorKey . center . relativeMinor  $ k


-- | Returns the relative 'Major' 'Key' of the specified 'Key'.
relativeMajor :: Key -> Key
relativeMajor k@(Key Major n) = k
relativeMajor k = majorKey . head . rotate 5 . notes . scale $ k
  where rotate 0 xs = xs
        rotate n xs = rotate (n-1) (last xs : init xs)


-- | Returns the relative 'Minor' 'Key' of the specified 'Key'.
relativeMinor :: Key -> Key
relativeMinor k = minorKey . fromJust . submediant . scale . relativeMajor $ k


-- | Returns the 'Signature' of the  specified 'Key'.
signature :: Key -> Signature
signature = Signature 


-- | Returns the details of the  specified 'Key':
--
-- (1) The 'Accidental' that is used in the 'Key' 'Signature'.
--
-- (2) The number of those 'Accidental's used in the 'Key' 'Signature'.
sigDetails :: Signature -> (Accidental, Int)
sigDetails s = 
  case ns of
    []       -> (Natural, 0)
    ns@(n:_) -> (accidental n, length ns)
  where ns = notes s

