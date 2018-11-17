{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Run where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Env

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: proxy kv -> AssocValue kv -> RIO Env ()
