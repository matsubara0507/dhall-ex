{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Format where

import           RIO

import           Dhall.Ex.Env (Env)
import qualified Dhall.Parser as Dhall

run :: FilePath -> RIO Env ()
run path = do
  logDebug $ fromString ("read file: " <> path)
  txt <- readFileUtf8 path
  case Dhall.exprFromText path txt of
    Right expr -> logInfo $ displayShow expr
    Left e     -> logError $ displayShow e
