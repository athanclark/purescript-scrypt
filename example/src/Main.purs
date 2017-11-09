module Main where

import Crypto.Scrypt (SCRYPT, scrypt)
import Data.TextEncoder (encodeUtf8)

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Unsafe.Coerce (unsafeCoerce)


main :: _
main = do
  log "Hello sailor!"

  void $ runAff (\e -> log $ unsafeCoerce e) (scrypt
    { password: encodeUtf8 "test"
    , salt: encodeUtf8 "salt"
    , n: 1024
    , r: 8
    , p: 1
    , dkLen: 32
    , onProgress: \p -> log $ "progress: " <> show p
    })
