module Main where

import Crypto.Scrypt (SCRYPT, scrypt)
import Data.TextEncoder (encodeUtf8)

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)

main :: forall e. Eff (console :: CONSOLE, scrypt :: SCRYPT | e) Unit
main = do
  log "Hello sailor!"

  scrypt
    { password: encodeUtf8 "test"
    , salt: encodeUtf8 "salt"
    , n: 1024
    , r: 8
    , p: 1
    , dkLen: 32
    , onError: error
    , onProgress: \p -> log $ "progress: " <> show p
    , onComplete: \key -> log $ show key
    }
