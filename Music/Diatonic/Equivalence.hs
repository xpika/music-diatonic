-- | The module implements a typeclass for enharmonic equivalence. 

module Music.Diatonic.Equivalence where


class Equiv a where 
  -- | Returns 'True' if both as are enharmonically equivalent, 'False' otherwise.
  equiv :: a -> a -> Bool
