{-# LANGUAGE DataKinds        #-}
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
  run' :: proxy kv -> AssocValue kv -> RIO Env ()

runWithOnly ::
  (FilePath -> a -> Export -> RIO Env ()) -> FilePath -> a -> RIO Env ()
runWithOnly act dir opts = do
  config <- asks (view #config)
  only   <- asks (view #only)
  case findExportByName (config ^. #exports) =<< only of
    Just export -> act dir opts export
    Nothing     -> mapM_ (act dir opts) (config ^. #exports)
