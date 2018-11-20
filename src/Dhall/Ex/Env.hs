{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Env where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Config (Config)

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
