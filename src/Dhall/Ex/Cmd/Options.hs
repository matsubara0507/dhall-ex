{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Options where

import           RIO
import qualified RIO.Text         as Text

import           Data.Extensible
import           Dhall.Ex.Cmd.Run
import qualified Dhall.Ex.Sort    as Sort

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "sort" >: Text
   , "echo" >: Text
   ]

instance Run ("sort" >: Text) where
  run' _ = Sort.run . Text.unpack

instance Run ("echo" >: Text) where
  run' _ txt = do
    config <- asks (view #config)
    logDebug $ displayShow config
    logInfo  $ display txt
