module Types where

type Octave   = Int
type Duration = Int
type Alter    = Int
type Voice    = Int
type StemUp   = Bool
type Staff    = Int

type Pitch    = (Octave, Step, Maybe Alter)

data Beam     = Beam Int String

data Step       = C | D | E | F | G | A | B | H
                deriving (Eq, Ord, Show) 

data NoteType   = Sixteenth | Eighth | Quarter | Half | Whole
                deriving (Eq, Ord)
 
data Accidental = Natural | Flat | Sharp
                deriving (Eq, Ord)

data Note = Rest Duration | Note Pitch Duration (Maybe Accidental)
          deriving (Eq, Ord)

instance Show Note where
  show (Note (o,s,_) d ma) = case ma of
    Nothing        -> (show s) ++ ' ':';':(show o) ++ ';':(show d)
    (Just Natural) -> (show s) ++ '~':';':(show o) ++ ';':(show d)
    (Just Sharp)   -> (show s) ++ '#':';':(show o) ++ ';':(show d)
    (Just Flat)    -> (show s) ++ 'b':';':(show o) ++ ';':(show d)
