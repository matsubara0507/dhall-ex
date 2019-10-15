{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Run where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Config (Export, findExportByName)
import           Dhall.Ex.Env

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: proxy kv -> TargetOf kv -> RIO Env ()

runWithOnly ::
  (FilePath -> a -> Export -> RIO Env ()) -> FilePath -> a -> RIO Env ()
runWithOnly act dir opts = runWithOnly' (flip act opts) dir

runWithOnly' ::
  (FilePath -> Export -> RIO Env ()) -> FilePath -> RIO Env ()
runWithOnly' act dir = do
  config <- asks (view #config)
  asks (view #only) >>= \case
    Nothing   -> mapM_ (act dir) (config ^. #exports)
    Just name -> case findExportByName (config ^. #exports) name of
      Just export -> act dir export
      Nothing     -> logError $ display ("undefined name: " <> name)
