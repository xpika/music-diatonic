{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module implements harmonies.

module Music.Diatonic.Harmony (
    Harmony,
    harmony, harmony7, chords, showRoman
  ) where 
 


import Music.Diatonic
import Music.Diatonic.Scale
import Music.Diatonic.Chord
import Data.Char (toLower)


data HarmonyType = Triads | Sevenths
                   deriving (Eq, Show)


data Harmony = Harmony HarmonyType Scale 
             deriving (Eq)


instance Show Harmony where
  show (Harmony Triads s) = show s ++ " Harmony"
  show (Harmony Sevenths s) = show s ++ " Sevenths Harmony"


instance Deg Harmony Chord where
  first = head . chords
  degrees h = map (\c -> (degree (tonic . scale $ h) (root c), c)) (chords h)
  

instance Qual Harmony where 
  quality (Harmony t s) = quality s


instance Scl Harmony where
  scale (Harmony t s) = s


-- | Returns a Roman notation string representing the 'Degree' of the 'Chord'.
showRoman :: Chord -> Degree -> String
showRoman c d = prefix ++ roman n ++ suffix
  where (n:prefix) = reverse . show $ d
        suffix = drop (length . show . root $ c) (show c)
        roman '1' = "I"
        roman '2' = "II"
        roman '3' = "III"
        roman '4' = "IV"
        roman '5' = "V"
        roman '6' = "VI"
        roman '7' = "VII"


-- | Returns the 'Harmony' for the specified 'Scale'.
harmony :: Scale -> Harmony
harmony s = Harmony Triads s


-- | Returns the 7ths 'Harmony' for the specified 'Scale'.
harmony7 :: Scale -> Harmony
harmony7 s = Harmony Sevenths s


-- | Returns the 'Chord's that compose the specified 'Harmony'.
chords :: Harmony -> [Chord]
chords (Harmony t s) = zipWith ($) (cs t . quality $ s) (notes s)
  where cs Triads   Major = 
          [majorChord, minorChord, minorChord, majorChord, majorChord, minorChord, diminishedChord]
        cs Triads   Minor | s == (minorScale . tonic $ s) = 
          [minorChord, diminishedChord, majorChord, minorChord, minorChord, majorChord, majorChord]
        cs Triads   Minor | s == (minorHarmonicScale . tonic $ s) = 
          [minorChord, diminishedChord, augmentedChord, minorChord, majorChord, majorChord, diminishedChord]
        cs Triads   Minor | s == (minorMelodicScale . tonic $ s) = 
          [minorChord, minorChord, augmentedChord, majorChord, majorChord, diminishedChord, diminishedChord]

        cs Sevenths Major = 
          [major7thChord, minor7thChord, minor7thChord, major7thChord, dominant7thChord, minor7thChord, minor7thFlat5thChord]
        cs Sevenths Minor | s == (minorScale . tonic $ s) = 
          [minor7thChord, minor7thFlat5thChord, major7thChord, minor7thChord, minor7thChord, major7thChord, dominant7thChord]
        cs Sevenths Minor | s == (minorHarmonicScale . tonic $ s) = 
          [minorMajor7thChord, minor7thFlat5thChord, augmentedMajor7thChord, minor7thChord, dominant7thChord, major7thChord, diminished7thChord]
        cs Sevenths Minor | s == (minorMelodicScale . tonic $ s) = 
          [minorMajor7thChord, minor7thChord, augmentedMajor7thChord, dominant7thChord, dominant7thChord, minor7thFlat5thChord, minor7thFlat5thChord]

