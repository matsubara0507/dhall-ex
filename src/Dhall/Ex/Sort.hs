{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Ex.Sort where

import           RIO

import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import           Dhall.Core                            (Expr (..))
import qualified Dhall.Core                            as Dhall
import           Dhall.Ex.Env                          (Env)
import qualified Dhall.Import                          as Dhall
import qualified Dhall.Map                             as Dhall
import qualified Dhall.Parser                          as Dhall

run :: FilePath -> RIO Env ()
run path = do
  logDebug $ fromString ("read file: " <> path)
  txt <- readFileUtf8 path
  case Dhall.exprAndHeaderFromText path txt of
    Left e               -> logError $ displayShow e
    Right (header, expr) -> do
      expr' <- liftIO $ Dhall.load expr
      logDebug $ display ("expr: " <> tshow expr')
      case Dhall.denote expr' of
        (Annot _ t) -> writeWithPrettyAndSort path (header, expr) t
        _           -> writeWithPretty path (header, expr)

writeWithPrettyAndSort :: (Show s, Show a, Pretty.Pretty a)
  => FilePath -> (Text, Expr s a) -> Expr s b -> RIO Env ()
writeWithPrettyAndSort path (header, expr) t =
  case expr `sortFieldsBy` Dhall.denote t of
    Left key    -> logError $ display ("not found key: " <> key)
    Right expr' -> writeWithPretty path (header, expr')

sortFieldsBy :: Expr s a -> Expr s b -> Either Text (Expr s a)
sortFieldsBy e t = case (e, t) of
  (Note s e', _)               -> Note s <$> (e' `sortFieldsBy` t)
  (Annot e' t', _)             -> Annot <$> (e' `sortFieldsBy` t) <*> pure t'
  (RecordLit r, Record rt)     -> RecordLit <$> (r `sortKeyBy` rt)
  (ListLit lt es, App List t') -> ListLit lt <$> traverse (`sortFieldsBy` t') es
  _                            -> pure $ e

sortKeyBy
  :: Dhall.Map Text (Expr s a)
  -> Dhall.Map Text (Expr s b)
  -> Either Text (Dhall.Map Text (Expr s a))
sortKeyBy r = Dhall.traverseWithKey $ \k t ->
  (maybe (Left k) pure $ Dhall.lookup k r) >>= (`sortFieldsBy` t)

writeWithPretty :: (Show s, Show a, Pretty.Pretty a)
  => FilePath -> (Text, Expr s a) -> RIO Env ()
writeWithPretty path (header, expr) = do
  logDebug $ display ("write expr: " <> tshow expr)
  let doc = Pretty.pretty header <> Pretty.pretty expr
  writeFileUtf8 path $ Pretty.renderStrict (Pretty.layoutSmart opts doc)

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
