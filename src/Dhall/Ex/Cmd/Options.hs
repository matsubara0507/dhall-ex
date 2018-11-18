{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Options where

import           RIO
import qualified RIO.Text         as Text

import           Data.Extensible
import           Dhall.Ex.Cmd.Run
import qualified Dhall.Ex.Format  as Format

type Options = Record
  '[ "verbose" >: Bool
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "format" >: Text
   ]

instance Run ("format" >: Text) where
  run' _ = Format.run . Text.unpack
