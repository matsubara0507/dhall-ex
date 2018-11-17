{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Cmd.Run where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Cmd.Options
import           Dhall.Ex.Env

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: nil
    runRIO env run'

run' :: RIO Env ()
run' = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
