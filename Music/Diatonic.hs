-- | The module is used as a convinient starting point. It re-exports:
--
-- * "Music.Diatonic.Note"
--
-- * "Music.Diatonic.Interval"
--
-- * "Music.Diatonic.Degree"
--
-- * "Music.Diatonic.Quality"
--
-- * "Music.Diatonic.Equivalence"

module Music.Diatonic (
    module Music.Diatonic.Note,
    module Music.Diatonic.Interval,
    module Music.Diatonic.Degree,
    module Music.Diatonic.Quality,
    module Music.Diatonic.Equivalence
  ) where


import Music.Diatonic.Note
import Music.Diatonic.Interval
import Music.Diatonic.Quality
import Music.Diatonic.Equivalence
import Music.Diatonic.Degree
