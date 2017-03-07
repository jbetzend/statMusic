{-# LANGUAGE OverloadedStrings #-}

module Parser
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

import Types

parseNote :: Parser Note
parseNote = undefined

parsePitches :: Parser [Pitch]
parsePitches = many1 parseFoo

parseFoo :: Parser Pitch
parseFoo = do skipWhile (/= '<')
              parsePitch <|> (anyChar >> pure ('Z',0))

parsePitch :: Parser Pitch
parsePitch = do string "<pitch>"
                skipSpace
                s <- parseStep
                skipSpace
                o <- parseOctave
                skipSpace
                string "</pitch>"
                pure (s,o)

-- ex:  <step>C</step>
parseStep :: Parser Step
parseStep = do string "<step>"
               c <- letter_ascii
               string "</step>"
               pure c

-- ex: <octave>4</octave>
parseOctave :: Parser Octave
parseOctave = do string "<octave>"
                 o <- decimal
                 string "</octave>"
                 pure o
