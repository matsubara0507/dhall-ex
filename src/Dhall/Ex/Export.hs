{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Export
  ( module X
  , init
  , initExport
  , cloneRepo
  , constructRepoUrl
  , workDir
  ) where

import           RIO
import           RIO.Directory

import           Dhall.Ex.Config          (Export, ghToken)
import           Dhall.Ex.Env
import           Dhall.Ex.Export.Build    as X (build)
import           Dhall.Ex.Export.Checkout as X (Checkout, checkout)
import           Dhall.Ex.Export.Deploy   as X (Deploy, deploy)
import qualified Shelly                   as Sh

init :: FilePath -> RIO Env ()
init dir = do
  logDebug $ fromString ("creat work directory if missing: " <> dir)
  createDirectoryIfMissing True dir
  config <- asks (view #config)
  mapM_ (initExport dir) $ config ^. #exports

initExport :: FilePath -> Export -> RIO Env ()
initExport dir conf = do
  logDebug $ display ("init export: " <> tshow conf)
  case conf ^. #repo of
    Just repo -> cloneRepo dir repo
    Nothing   -> pure ()

cloneRepo :: FilePath -> Text -> RIO Env ()
cloneRepo dir repo = do
  logDebug $ display ("clone repository: " <> repo)
  config <- asks (view #config)
  let repo' = constructRepoUrl repo $ config ^. ghToken
      dir'  = mconcat [fromString dir, "/", repo]
  Sh.shelly $ Sh.command1_ "git" [] "clone" ["--depth", "1", repo', dir']

constructRepoUrl :: Text -> Maybe Text -> Text
constructRepoUrl repo = mconcat . \case
  (Just token') -> [ "https://", token', "@github.com/", repo ]
  Nothing       -> [ "https://",          "github.com/", repo ]

workDir :: FilePath
workDir = ".dhall-ex"
