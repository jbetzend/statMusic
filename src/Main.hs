module Main where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, pack)
import Data.Map.Strict
import Data.List (sortBy)
import Data.Ord (comparing)

import System.Environment

import Parser
import Types



foobar :: ByteString -> [Pitch]
foobar con = case parseOnly parsePitches con of
  (Left err) -> []
  (Right ps) -> Prelude.filter (/= ('Z',0)) ps 

snafu :: [Pitch] -> Map Pitch Int
snafu []     = empty
snafu (p:ps) = let m = snafu ps
                in if member p m
                   then adjust succ p m
                   else insert p 1 m

main :: IO ()
main = do args <- getArgs
          let file = head args
          putStrLn $ "Reading file " ++ file
          contents <- readFile file
          let foos = foobar (pack contents)
          putStrLn $ "Parsed " ++ show (length foos) ++ " note pitches:"
          putStrLn $ show $ sortBy (comparing snd) $ toList $ snafu foos 
