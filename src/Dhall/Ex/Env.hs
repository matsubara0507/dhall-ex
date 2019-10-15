{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Env where

import           RIO

import           Data.Extensible
import           Dhall.Ex.Config (Config)

type Env = Record
  '[ "logger"  >: LogFunc
   , "config"  >: Config
   , "only"    >: Maybe Text
   , "verbose" >: Bool
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

class HasVerboseFlag env where
  isVerbose :: env -> Bool

instance Lookup xs "verbose" Bool => HasVerboseFlag (Record xs) where
  isVerbose = view #verbose
