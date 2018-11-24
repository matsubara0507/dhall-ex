{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Export.Deploy where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text        as Text

import           Data.Extensible
import           Dhall.Ex.Config (Export)
import           Dhall.Ex.Env
import           Dhall.Ex.Utils

type Deploy = Record
  '[ "branch" >: Text
   ]

deploy :: FilePath -> Deploy -> Export -> RIO Env ()
deploy dir opts conf = do
  logDebug $ display ("deploy export: " <> tshow conf)
  env <- ask
  forM_ (conf ^. #repo) $ \repo -> do
    let path = dir </> Text.unpack repo
    withCurrentDirectory path $ shelly' env $ whenM existChangesFile $ do
      gitCheckout True (opts ^. #branch)
      gitCommitAllChanges "Update config by dhall-ex"
      gitPush True (opts ^. #branch)
      gitCheckout False "-"
