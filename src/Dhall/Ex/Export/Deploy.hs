{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Export.Deploy where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List        as L
import qualified RIO.Text        as Text

import           Data.Extensible
import           Dhall.Ex.Config (Export)
import           Dhall.Ex.Env
import           Dhall.Ex.Utils

type Deploy = Record
  '[ "branch" >: Text
   , "only"   >: Maybe Text
   ]

deploy :: FilePath -> Deploy -> RIO Env ()
deploy dir opts = do
  config <- asks (view #config)
  case findExportByName (config ^. #exports) =<< opts ^. #only of
    Just export -> deployExport dir opts export
    Nothing     -> mapM_ (deployExport dir opts) (config ^. #exports)

findExportByName :: [Export] -> Text -> Maybe Export
findExportByName exports name = L.find (\e -> e ^. #name == name) exports

deployExport :: FilePath -> Deploy -> Export -> RIO Env ()
deployExport dir opts conf = do
  logDebug $ display ("deploy export: " <> tshow conf)
  env <- ask
  forM_ (conf ^. #repo) $ \repo -> do
    let path = dir </> Text.unpack repo
    withCurrentDirectory path $ shelly' env $ whenM existChangesFile $ do
      gitCheckout True (opts ^. #branch)
      gitCommitAllChanges "Update config by dhall-ex"
      gitPush True (opts ^. #branch)
      gitCheckout False "-"
