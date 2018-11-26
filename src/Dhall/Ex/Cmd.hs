{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Cmd
    ( module X
    , run
    ) where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Cmd.Options as X
import           Dhall.Ex.Cmd.Run     as X
import           Dhall.Ex.Config      (readConfig)

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger  @= logger
           <: #config  @= config
           <: #only    @= (opts ^. #only)
           <: #verbose @= (opts ^. #verbose)
           <: nil
    runRIO env $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)
