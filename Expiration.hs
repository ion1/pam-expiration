module Expiration (newExpiration) where

import Shadow.Types

import Control.Monad
import Data.Time.Clock.POSIX (POSIXTime, posixDayLength)

newExpiration :: POSIXTime -> Integer -> ShadowEntry -> Maybe Integer
newExpiration now expireDays entry = do
  unless (passwordEnabled entry) $ fail "Password disabled"

  -- Nothing if expiration is not enabled for the user.
  curExpireDate <- expireDate entry

  when (curExpireDate < today)          $ fail "Account expired"
  when (curExpireDate == newExpireDate) $ fail "No change needed"

  return newExpireDate

  where
    today = floor (now / posixDayLength)
    newExpireDate = today + expireDays
