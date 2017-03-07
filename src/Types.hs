module Types where

type Octave = Int
type Duration = Int
type Step = Char

type Pitch = (Step, Octave)

data Accidental = Natural | Flat | Sharp

data Note = Note Pitch Duration
