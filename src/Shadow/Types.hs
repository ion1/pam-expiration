module Shadow.Types
  ( ShadowEntry (..)
  , passwordEnabled
  , shadowEntryToString
  )
where

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
  deriving Show

passwordEnabled entry =
  case password entry of
    '*':_ -> False  -- Disabled.
    '!':_ -> False  -- Disabled.
    ""    -> False  -- Empty.
    _     -> True

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
