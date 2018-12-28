{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import           Prelude

import           Data.Extensible
import           Data.Functor.Identity
import           Data.String           (fromString)
import qualified Dhall                 as Dhall
import           GHC.TypeLits

instance Forall (KeyValue KnownSymbol (Instance1 Dhall.Interpret h)) xs => Dhall.Interpret (Field h :* xs) where
  autoWith opts = Dhall.record $ hgenerateFor
    (Proxy @ (KeyValue KnownSymbol (Instance1 Dhall.Interpret h))) $ \m ->
      let k = (fromString . symbolVal . proxyAssocKey) m in
      Dhall.field k (fmap Field (Dhall.autoWith opts))

deriving instance Dhall.Interpret (h (AssocValue kv)) => Dhall.Interpret (Field h kv)
deriving instance Dhall.Interpret a => Dhall.Interpret (Identity a)
