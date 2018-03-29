module Crypto.Scrypt
  ( SCRYPT, ScryptParams, scrypt
  ) where

import Prelude
import Data.Either (Either (..))
import Data.ArrayBuffer.Types (Uint8Array)
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1, runEffFn1)
import Control.Monad.Eff.Exception (error)



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
  , onProgress :: Number -> Eff eff Unit
  }


scrypt :: forall eff. ScryptParams (scrypt :: SCRYPT | eff) -> Aff (scrypt :: SCRYPT | eff) Uint8Array
scrypt {password,salt,n,r,p,dkLen,onProgress} = makeAff \resolve -> nonCanceler <$ runEffFn1 scryptImpl
  { password, salt, n, r, p, dkLen
  , onError: mkEffFn1 (resolve <<< Left <<< error)
  , onProgress: mkEffFn1 onProgress
  , onComplete: mkEffFn1 (resolve <<< Right)
  }
