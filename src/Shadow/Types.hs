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

module Shadow.Types
  ( ShadowEntry (..)
  , passwordEnabled
  , shadowEntryToString
  )
where

import Control.Applicative
import Control.Monad
import Data.List (intercalate)

data ShadowEntry = ShadowEntry { loginName    :: String
                               , password     :: String
                               , lastChange   :: Maybe Integer
                               , minAge       :: Maybe Integer
                               , maxAge       :: Maybe Integer
                               , warnDays     :: Maybe Integer
                               , inactiveDays :: Maybe Integer
                               , expireDate   :: Maybe Integer
                               , reserved     :: String
                               }
  deriving (Eq, Show)

passwordEnabled =
  liftM2 (&&) (null . deleteValidChars . password)
              (not . null . password)
  where
    deleteValidChars = filter (not . (`elem` validChars))
    validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "./$"

shadowEntryToString entry = intercalate ":" fields
  where
    fields = map ($ entry) fs

    fs = [ loginName
         , password
         , showMaybeInteger . lastChange
         , showMaybeInteger . minAge
         , showMaybeInteger . maxAge
         , showMaybeInteger . warnDays
         , showMaybeInteger . inactiveDays
         , showMaybeInteger . expireDate
         , reserved
         ]

    showMaybeInteger :: Maybe Integer -> String
    showMaybeInteger = maybe "" show
