{-# LANGUAGE FlexibleContexts #-}

module Dhall.Ex.Utils where

import           RIO
import qualified RIO.Text as Text

import qualified Shelly   as Sh

gitStatus :: Sh.Sh Text
gitStatus = Sh.command1 "git" [] "status" []

gitCheckout :: Bool -> Text -> Sh.Sh ()
gitCheckout isNew branch = Sh.command1_ "git" [] "checkout" opts
  where
    opts = if isNew then ["-b", branch] else [branch]

gitCommit :: Text -> Sh.Sh ()
gitCommit message = Sh.command1_ "git" [] "commit" ["-m", message]

gitAdd :: Text -> Sh.Sh ()
gitAdd path = Sh.command1_ "git" [] "add" [path]

gitPush :: Bool -> Text -> Sh.Sh ()
gitPush isNew branch = Sh.command1_ "git" [] "push" opts
  where
    opts = if isNew then ["-u", "origin", branch] else ["origin", branch]

gitCommitAllChanges :: Text -> Sh.Sh ()
gitCommitAllChanges message = do
  gitAdd "."
  gitCommit message

existChangesFile :: Sh.Sh Bool
existChangesFile = do
  txt <- gitStatus
  pure . not $ "nothing to commit, working tree clean\n" `Text.isSuffixOf` txt

shelly'
  :: (MonadIO m, HasLogFunc env, HasCallStack)
  => env -> Sh.Sh a -> m a
shelly' env = Sh.shelly -- . Sh.silently
  . Sh.log_stdout_with (runRIO env . logDebug . display)
  . Sh.log_stderr_with (runRIO env . logDebug . display)