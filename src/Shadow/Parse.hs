module Shadow.Parse (shadowFile) where

import Shadow.Types (ShadowEntry (..))

import Control.Monad (ap, liftM)
import Text.ParserCombinators.Parsec.Char (char, noneOf, oneOf)
import Text.ParserCombinators.Parsec.Combinator (eof, many1, optionMaybe)
import Text.ParserCombinators.Parsec.Prim (Parser, many, (<|>))

shadowFile :: Parser [ShadowEntry]
shadowFile = do
  entries <- many shadowEntry
  eof
  return entries

shadowEntry :: Parser ShadowEntry
shadowEntry =
  return ShadowEntry
    `ap` andSep shadowString       -- loginName
    `ap` andSep shadowString       -- password
    `ap` andSep shadowMaybeInteger -- lastChange
    `ap` andSep shadowMaybeInteger -- minAge
    `ap` andSep shadowMaybeInteger -- maxAge
    `ap` andSep shadowMaybeInteger -- warnDays
    `ap` andSep shadowMaybeInteger -- inactiveDays
    `ap` andSep shadowMaybeInteger -- expireDate
    `ap` andEnd shadowString       -- reserved

shadowString :: Parser String
shadowString = many $ noneOf ":\n"

shadowMaybeInteger :: Parser (Maybe Integer)
shadowMaybeInteger = optionMaybe . liftM read . many1 . oneOf $ ['0'..'9']

andSep :: Parser a -> Parser a
andSep p = do
  result <- p
  char ':'
  return result

andEnd :: Parser a -> Parser a
andEnd p = do
  result <- p
  (char '\n' >> return ()) <|> eof
  return result
