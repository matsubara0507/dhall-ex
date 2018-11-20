{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Export.Build where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text        as Text

import qualified Data.Aeson      as JSON
import qualified Data.Yaml       as YAML
import qualified Dhall           as Dhall
import           Dhall.Ex.Config (Export)
import           Dhall.Ex.Env
import qualified Dhall.JSON      as Dhall

build :: FilePath -> RIO Env ()
build dir = mapM_ (buildExport dir) =<< asks (view #exports . view #config)

buildExport :: FilePath -> Export -> RIO Env ()
buildExport dir conf = do
  logDebug $ display ("build export: " <> tshow conf)
  config <- asks (view #config)
  let action = maybe buildExportToLocal (buildExportToRepo dir) (conf ^. #repo)

  forM_ (conf ^. #paths) $ \path -> do
    let path' = (config ^. #root) </> path <.> "dhall"
    logDebug $ fromString ("read file: " <> path')
    txt  <- readFileUtf8 path'
    expr <- withCurrentDirectory (takeDirectory path') (liftIO $ Dhall.inputExpr txt)
    logDebug $ display ("expr: " <> tshow expr)
    case Dhall.dhallToJSON expr of
      Left err  -> logError $ display ("parse error: " <> tshow err)
      Right val -> action ((config ^. #root) </> path) val

buildExportToRepo :: FilePath -> Text -> FilePath -> JSON.Value -> RIO Env ()
buildExportToRepo dir repo path = buildExportToLocal path'
  where
    path' = dir </> Text.unpack repo </> takeFileName path

buildExportToLocal :: FilePath -> JSON.Value -> RIO Env ()
buildExportToLocal path val = do
  logDebug $ fromString ("write file: " <> path)
  if | isJSON path -> liftIO $ JSON.encodeFile path val
     | isYAML path -> liftIO $ YAML.encodeFile path val
     | otherwise   -> logError $ fromString ("Known extension: " <> path)

isJSON, isYAML :: FilePath -> Bool
isJSON path = "json" `isExtensionOf` path
isYAML path = "yml" `isExtensionOf` path || "yaml" `isExtensionOf` path
