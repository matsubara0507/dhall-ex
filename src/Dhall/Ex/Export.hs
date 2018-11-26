{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Export
  ( module X
  , init
  , cloneRepo
  , constructRepoUrl
  , workDir
  ) where

import           RIO

import           Dhall.Ex.Config        (Export, ghToken)
import           Dhall.Ex.Env
import           Dhall.Ex.Export.Build  as X (build)
import           Dhall.Ex.Export.Deploy as X (Deploy, deploy)
import           Dhall.Ex.Export.Git    as X (Checkout, checkout, pull)
import qualified Shelly                 as Sh

init :: FilePath -> Export -> RIO Env ()
init dir conf = do
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
