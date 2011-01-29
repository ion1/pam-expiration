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

module Main where

import Expiration
import Shadow (shadowFile)

import Control.Applicative
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment
import System.Exit
import System.Log.Handler.Syslog
import System.Log.Logger
import System.Process
import Text.ParserCombinators.Parsec (parse)

expireDays = 180
logLevel   = DEBUG

main = do
  syslog <- openlog "pam-expiration" [PID] AUTH DEBUG
  updateGlobalLogger rootLoggerName
                     (setLevel logLevel . addHandler syslog)

  main' `catch` (\e -> errorM rootLoggerName (show e) >> exitFailure)

main' = do
  args    <- getArgs
  service <- getEnv "PAM_SERVICE"
  user    <- getEnv "PAM_USER"

  when (length args /= 1) $ error "Expected exactly one argument"
  let moduleType = head args

  case (moduleType, service) of
    ("auth",     _)      -> maybeChangeExpiration user
    ("ses_open", "sshd") -> maybeChangeExpiration user
    _                    -> return ()

maybeChangeExpiration user = do
  shadow  <- readProcess "/usr/bin/getent" ["shadow", "--", user] ""
  entries <- eitherError $ parse shadowFile "shadow" shadow
  now     <- getPOSIXTime

  when (length entries /= 1) 
       (error $ "Expected exactly one shadow entry (user " ++ show user ++ ")")

  either (debugM rootLoggerName
          . showString "Not changing expire date of user "
          . shows user . showString ": ")
         (changeExpiration user)
         (newExpiration now expireDays $ head entries)

  where
    eitherError = either (ioError . userError . show) return

changeExpiration user n = do
  noticeM rootLoggerName
          ("Setting expire date of user " ++ show user ++ " as " ++ show n)

  readProcess "/usr/bin/chage" ["--expiredate", show n, "--", user] ""
  return ()
