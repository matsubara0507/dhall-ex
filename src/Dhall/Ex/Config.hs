{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Dhall.Ex.Config where

import           RIO
import qualified RIO.List        as L

import           Data.Extensible
import qualified Dhall           as Dhall
import           Orphans         ()

type Config = Record
  '[ "GH_TOKEN" >: Maybe Text
   , "root"     >: FilePath
   , "exports"  >: [Export]
   ]

type Export = Record
  '[ "name"  >: Text
   , "repo"  >: Maybe Text
   , "paths" >: [FilePath]
   ]

readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
readConfig path = (liftIO . Dhall.input Dhall.auto) =<< readFileUtf8 path

ghToken :: FieldOptic "GH_TOKEN"
ghToken = itemAssoc (Proxy @ "GH_TOKEN")

findExportByName :: [Export] -> Text -> Maybe Export
findExportByName exports name = L.find (\e -> e ^. #name == name) exports
