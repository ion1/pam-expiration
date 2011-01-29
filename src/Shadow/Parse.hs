{-
  pam-expiration – Expire inactive users

  Copyright © 2011 Johan Kiviniemi <devel@johan.kiviniemi.name>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

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
