{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Options where

import           RIO
import qualified RIO.Text         as Text

import           Data.Extensible
import           Dhall.Ex.Cmd.Run
import qualified Dhall.Ex.Export  as Export
import qualified Dhall.Ex.Sort    as Sort

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "only"    >: Maybe Text
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "sort"     >: Text
   , "echo"     >: Text
   , "init"     >: ()
   , "build"    >: ()
   , "deploy"   >: Export.Deploy
   , "checkout" >: Export.Checkout
   ]

instance Run ("sort" >: Text) where
  run' _ = Sort.run . Text.unpack

instance Run ("echo" >: Text) where
  run' _ txt = do
    config <- asks (view #config)
    logDebug $ displayShow config
    logInfo  $ display txt

instance Run ("init" >: ()) where
  run' _ _ = Export.init Export.workDir

instance Run ("build" >: ()) where
  run' _ _ = Export.build Export.workDir

instance Run ("deploy" >: Export.Deploy) where
  run' _ = runWithOnly Export.deploy Export.workDir

instance Run ("checkout" >: Export.Checkout) where
  run' _ = runWithOnly Export.checkout Export.workDir
