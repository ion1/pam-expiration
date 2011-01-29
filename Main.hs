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

main = do
  syslog <- openlog "pam-expiration" [PID, PERROR] AUTH INFO
  updateGlobalLogger rootLoggerName $ addHandler syslog

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
       (error $ "Expected exactly one shadow entry (user " ++ user ++ ")")

  let entry = head entries
  maybe (noticeM rootLoggerName ("Not changing expire date of user " ++ user))
        (changeExpiration user)
        (newExpiration now expireDays entry)

  where
    eitherError = either (ioError . userError . show) return

changeExpiration user n = do
  noticeM rootLoggerName
          ("Setting expire date of user " ++ user ++ " as " ++ show n)

  readProcess "/usr/bin/chage" ["--expiredate", show n, "--", user] ""
  return ()
