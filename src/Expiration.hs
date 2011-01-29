module Expiration (newExpiration) where

import Shadow.Types

import Control.Monad.Error
import Data.Time.Clock.POSIX (POSIXTime, posixDayLength)

newExpiration :: POSIXTime -> Integer -> ShadowEntry -> Either String Integer
newExpiration now expireDays entry = do
  unless (passwordEnabled entry) $ fail "Password disabled"

  curExpireDate <- maybe (fail "Expiration not enabled the user") return
                         (expireDate entry)

  when (curExpireDate < today)          $ fail "Account expired"
  when (curExpireDate == newExpireDate) $ fail "No change needed"

  return newExpireDate

  where
    today = floor (now / posixDayLength)
    newExpireDate = today + expireDays
