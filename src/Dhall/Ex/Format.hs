{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Format where

import           RIO

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Dhall.Core                 (Expr (..))
import qualified Dhall.Core                 as Dhall
import           Dhall.Ex.Env               (Env)
import qualified Dhall.Import               as Dhall
import qualified Dhall.Parser               as Dhall

run :: FilePath -> RIO Env ()
run path = do
  logDebug $ fromString ("read file: " <> path)
  txt <- readFileUtf8 path
  case Dhall.exprFromText path txt of
    Left e  -> logError $ displayShow e
    Right e -> do
      e' <- liftIO $ Dhall.load e
      logDebug $ display ("expr: " <> tshow e')
      case sortFieldsByType e' of
        Left key  -> logError $ display ("not found key: " <> key)
        Right e'' -> logInfo $ display (Dhall.pretty e'')

sortFieldsByType :: Expr s a -> Either Text (Expr s a)
sortFieldsByType = \case
  (Note s e)  -> Note s <$> sortFieldsByType e
  (Annot e t) -> Annot <$> (e `sortFieldsBy` Dhall.denote t) <*> pure t
  e           -> pure $ e

sortFieldsBy :: Expr s a -> Expr s a -> Either Text (Expr s a)
sortFieldsBy e t = case (e, t) of
  (Note s e', _)               -> Note s <$> (e' `sortFieldsBy` t)
  (RecordLit r, Record rt)     -> RecordLit <$> (r `sortKeyBy` rt)
  (ListLit lt es, App List t') -> ListLit lt <$> traverse (`sortFieldsBy` t') es
  _                            -> pure $ e

sortKeyBy
  :: InsOrdHashMap Text (Expr s a)
  -> InsOrdHashMap Text (Expr s a)
  -> Either Text (InsOrdHashMap Text (Expr s a))
sortKeyBy r = InsOrdHashMap.traverseWithKey $ \k t ->
  (maybe (Left k) pure $ InsOrdHashMap.lookup k r) >>= (`sortFieldsBy` t)
