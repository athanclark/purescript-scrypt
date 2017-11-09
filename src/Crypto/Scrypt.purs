module Crypto.Scrypt
  ( SCRYPT, ScryptParams, scrypt
  ) where

import Prelude
import Data.ArrayBuffer.Types (Uint8Array)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1, runEffFn1)



foreign import data SCRYPT :: Effect


foreign import scryptImpl :: forall eff. EffFn1 (scrypt :: SCRYPT | eff)
                  { password :: Uint8Array
                  , salt :: Uint8Array
                  , n :: Int
                  , r :: Int
                  , p :: Int
                  , dkLen :: Int
                  , onError :: EffFn1 (scrypt :: SCRYPT | eff) String Unit
                  , onProgress :: EffFn1 (scrypt :: SCRYPT | eff) Number Unit
                  , onComplete :: EffFn1 (scrypt :: SCRYPT | eff) Uint8Array Unit
                  }
                  Unit


type ScryptParams eff =
  { password :: Uint8Array
  , salt :: Uint8Array
  , n :: Int
  , r :: Int
  , p :: Int
  , dkLen :: Int
  , onError :: String -> Eff (scrypt :: SCRYPT | eff) Unit
  , onProgress :: Number -> Eff (scrypt :: SCRYPT | eff) Unit
  , onComplete :: Uint8Array -> Eff (scrypt :: SCRYPT | eff) Unit
  }


scrypt :: forall eff. ScryptParams eff -> Eff (scrypt :: SCRYPT | eff) Unit
scrypt {password,salt,n,r,p,dkLen,onError,onProgress,onComplete} = runEffFn1 scryptImpl
  { password, salt, n, r, p, dkLen
  , onError: mkEffFn1 onError
  , onProgress: mkEffFn1 onProgress
  , onComplete: mkEffFn1 onComplete
  }
