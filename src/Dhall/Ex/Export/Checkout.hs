{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Dhall.Ex.Export.Checkout where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text        as Text

import           Data.Extensible
import           Dhall.Ex.Config (Export)
import           Dhall.Ex.Env
import           Dhall.Ex.Utils

type Checkout = Record
  '[ "branch" >: Text
   , "new"    >: Bool
   ]

checkout :: FilePath -> Checkout -> Export -> RIO Env ()
checkout dir opts conf = do
  logDebug $ display ("checkout export: " <> tshow conf)
  env <- ask
  forM_ (conf ^. #repo) $ \repo -> do
    let path = dir </> Text.unpack repo
    withCurrentDirectory path $
      shelly' env $ gitCheckout (opts ^. #new) (opts ^. #branch)
