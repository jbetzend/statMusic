{-# LANGUAGE OverloadedStrings #-}

module Parser
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 hiding (D)

import Types

parseNotes :: Parser [Maybe Note]
parseNotes = many1 stuff
  where
    stuff = (Just <$> parseNote) <|> (anyChar *> pure Nothing)

parseNote :: Parser Note
parseNote = do string "<note"
               skipWhile (/= '>') 
               string ">"
               skipSpace
               p <- (Right <$> parsePitch) <|> (Left <$> (string "<rest/>"))
               skipSpace
               d <- parseDuration
               skipSpace
               v <- option Nothing (Just <$> parseVoice)
               skipSpace
               t <- option Nothing (Just <$> parseType)
               skipSpace
               dot <- option False ((string "<dot/>") >> pure True)
               skipSpace
               a <- option Nothing (Just <$> parseAccidental)
               skipSpace
               s <- option Nothing (Just <$> parseStemUp)
               skipSpace
               k <- option Nothing (Just <$> parseStaff)
               skipSpace
               b <- option [Nothing] (many1 (Just <$> parseBeam <* skipSpace))
               skipSpace
               n <- option Nothing (Just <$> parseNotations)
               skipSpace
               string "</note>"
               case p of
                 (Right p') -> pure $ Note p' d a
                 (Left  _ ) -> pure $ Rest d

parsePitch :: Parser Pitch
parsePitch = do string "<pitch>"
                skipSpace
                s <- parseStep
                skipSpace
                a <- option Nothing (Just <$> parseAlter)
                skipSpace
                o <- parseOctave
                skipSpace
                string "</pitch>"
                pure (o,s,a)

-- TODO: Expand?
parseNotations :: Parser ()
parseNotations = (string "<notations>") *> foo
  where
    foo :: Parser ()
    foo = ((string "</notations>") *> pure ()) <|> (anyChar *> foo)

-- ex: <beam number="1">end</beam>
parseBeam :: Parser Beam
parseBeam = do string "<beam number=\""
               n <- decimal
               string "\">"
               s <- (string "begin")
                    <|> (string "continue")
                    <|> (string "end")
                    <|> (string "backward hook")
               string "</beam>"
               pure (Beam n (show s))

-- ex: <stem>up</stem>
-- ex: <stem>down</stem>
parseStemUp :: Parser StemUp
parseStemUp = do string "<stem>"
                 s <- (string "up" >> pure True) <|> (string "down" >> pure False)
                 string "</stem>"
                 pure s

-- ex: <staff>2</staff>
parseStaff :: Parser Staff
parseStaff = do string "<staff>"
                s <- decimal
                string "</staff>"
                pure s

-- ex: <voice>1</voice>
parseVoice :: Parser Voice
parseVoice = do string "<voice>"
                v <- decimal
                string "</voice>"
                pure v

-- ex: <type>quarter</type>
parseType :: Parser NoteType
parseType = do string "<type>"
               t <-     ((string "whole")   >> pure Whole) 
                    <|> ((string "half")    >> pure Half) 
                    <|> ((string "quarter") >> pure Quarter)
                    <|> ((string "eighth")  >> pure Eighth)
                    <|> ((string "16th")    >> pure Sixteenth)
               string "</type>"
               pure t

-- ex: <alter>1</alter>
parseAlter :: Parser Alter
parseAlter = do string "<alter>"
                a <- decimal
                string "</alter>"
                pure a

-- ex:  <step>C</step>
parseStep :: Parser Step
parseStep = do string "<step>"
               c <- letter_ascii
               string "</step>"
               case show c of
                 "'C'" -> pure C
                 "'D'" -> pure D
                 "'E'" -> pure E
                 "'F'" -> pure F
                 "'G'" -> pure G
                 "'A'" -> pure A
                 "'B'" -> pure B
                 "'H'" -> pure H
                 k     -> error ("Step parsing failed: c=" ++ show k)

-- ex: <octave>4</octave>
parseOctave :: Parser Octave
parseOctave = do string "<octave>"
                 o <- decimal
                 string "</octave>"
                 pure o

-- ex: <duration>8</duration>
parseDuration :: Parser Duration
parseDuration = do string "<duration>"
                   d <- decimal
                   string "</duration>"
                   pure d

-- ex: <accidental>sharp</accidental>
parseAccidental :: Parser Accidental
parseAccidental = do string "<accidental>"
                     o <- (string "natural") <|> (string "sharp") <|> (string "flat")
                     string "</accidental>"
                     case o of
                       "natural" -> pure Natural
                       "sharp"   -> pure Sharp
                       "flat"    -> pure Flat
                       _         -> error "Accidental parsing failed"
