{-# LANGUAGE OverloadedLabels #-}

module Dhall.Ex.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           Dhall.Ex.Cmd.Options as X
import           Dhall.Ex.Cmd.Run     as X

data Cmd
  = PrintVersion
  | RunCmd Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunCmd opts
