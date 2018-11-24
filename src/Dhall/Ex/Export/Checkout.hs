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
import           Dhall.Ex.Config (Export, findExportByName)
import           Dhall.Ex.Env
import           Dhall.Ex.Utils

type Checkout = Record
  '[ "branch" >: Text
   , "only"   >: Maybe Text
   , "new"    >: Bool
   ]

checkout :: FilePath -> Checkout -> RIO Env ()
checkout dir opts = do
  config <- asks (view #config)
  case findExportByName (config ^. #exports) =<< opts ^. #only of
    Just export -> checkoutExport dir opts export
    Nothing     -> mapM_ (checkoutExport dir opts) (config ^. #exports)

checkoutExport :: FilePath -> Checkout -> Export -> RIO Env ()
checkoutExport dir opts conf = do
  logDebug $ display ("checkout export: " <> tshow conf)
  env <- ask
  forM_ (conf ^. #repo) $ \repo -> do
    let path = dir </> Text.unpack repo
    withCurrentDirectory path $
      shelly' env $ gitCheckout (opts ^. #new) (opts ^. #branch)
