{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Cmd.Options where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Cmd.Run

type Options = Record
  '[ "verbose" >: Bool
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "format" >: Text
   ]


instance Run ("format" >: Text) where
  run' _ _ = showNotImpl
