{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Dhall.Ex.Config
  ( Config
  , Export
  , readConfig
  ) where

import           RIO

import           Data.Extensible
import qualified Dhall           as Dhall
import           Orphans         ()

type Config = Record
  '[ "GH_TOKEN" >: Maybe Text
   , "root"     >: Text
   , "exports"  >: [Export]
   ]

type Export = Record
  '[ "name"  >: Text
   , "repo"  >: Maybe Text
   , "paths" >: [Text]
   ]

readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
readConfig path = (liftIO . Dhall.input Dhall.auto) =<< readFileUtf8 path
