module Crypto.Scrypt
  ( ScryptParams, scrypt
  ) where

import Prelude
import Data.Either (Either (..))
import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Effect.Exception (error)




foreign import scryptImpl :: EffectFn1
  { password :: Uint8Array
  , salt :: Uint8Array
  , n :: Int
  , r :: Int
  , p :: Int
  , dkLen :: Int
  , onError :: EffectFn1 String Unit
  , onProgress :: EffectFn1 Number Unit
  , onComplete :: EffectFn1 Uint8Array Unit
  }
  Unit


type ScryptParams =
  { password :: Uint8Array
  , salt :: Uint8Array
  , n :: Int
  , r :: Int
  , p :: Int
  , dkLen :: Int
  , onProgress :: Number -> Effect Unit
  }


scrypt :: ScryptParams -> Aff Uint8Array
scrypt {password,salt,n,r,p,dkLen,onProgress} = makeAff \resolve -> nonCanceler <$ runEffectFn1 scryptImpl
  { password, salt, n, r, p, dkLen
  , onError: mkEffectFn1 (resolve <<< Left <<< error)
  , onProgress: mkEffectFn1 onProgress
  , onComplete: mkEffectFn1 (resolve <<< Right)
  }
