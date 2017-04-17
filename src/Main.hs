module Main where

import Data.Maybe

import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Map.Strict
import Data.List (sortBy, foldl')
import Data.Ord (comparing)

import System.Environment

import Parser
import Types

import Debug.Trace

foobar :: ByteString -> [Note]
foobar con = case parseOnly parseNotes con of
  (Left err) -> []
  (Right ns) -> (forget . catMaybes) ns

forget :: [Note] -> [Note]
forget = fmap f
  where
    f (Note (s,o,_) d a) = Note (s,o,Nothing) 0 a
    f (Rest _          ) = Rest 0

snafu :: [Note] -> Map Note Int
snafu []     = defaultMap
  where
    defaultMap :: Map Note Int
    defaultMap = Prelude.foldr (\n -> insert n 0) empty allNotes

    allNotes :: [Note]
    allNotes = [Note (o,s,Nothing) 0 a | s <- [C,D,E,F,G,A,B,H], o <- [1..9], a <- accs]

    accs :: [Maybe Accidental]
    accs = [Nothing, (Just Flat), (Just Sharp), (Just Natural)]
snafu (n:ns) = let m = snafu ns
                in if member n m
                   then adjust succ n m
                   else insert n 1 m

present :: [(Note, Int)] -> String
present ns = unlines $ header:(Prelude.map transLine (Prelude.filter ((/= (Rest 0)) . fst) ns))
  where
    header :: String
    header = "step;octave;duration;amount"

    transLine :: (Note, Int) -> String
    transLine (no,n) = show no ++ ";" ++ show n

main :: IO ()
main = do args <- getArgs
          let file = head args
          putStrLn $ "Reading file " ++ file
          contents <- readFile file
          let foos = foobar (pack contents)
          putStrLn $ "Parsed " ++ show (length foos) ++ " notes."
          putStrLn $ "Writing results to output.csv"
          writeFile "output.csv" ((present . toList . snafu) foos) 
