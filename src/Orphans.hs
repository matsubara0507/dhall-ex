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
import qualified Dhall                 as Dhall

instance Forall (KeyTargetAre KnownSymbol (Instance1 Dhall.Interpret h)) xs => Dhall.Interpret (xs :& Field h) where
  autoWith opts = Dhall.record $ hgenerateFor
    (Proxy @ (KeyTargetAre KnownSymbol (Instance1 Dhall.Interpret h))) $ \m ->
      Dhall.field (stringKeyOf m) (fmap Field (Dhall.autoWith opts))

deriving instance Dhall.Interpret (h (TargetOf kv)) => Dhall.Interpret (Field h kv)
deriving instance Dhall.Interpret a => Dhall.Interpret (Identity a)
