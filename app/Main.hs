{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Paths_dhall_ex      as Meta
import           RIO

import           Data.Extensible
import           Dhall.Ex.Cmd
import qualified Dhall.Ex.Export     as Export
import           GHC.TypeLits
import           Options.Applicative
import qualified Version

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> version <**> helper)
         $ fullDesc
        <> header "dhall-ex - cli tool to support dhall"

options :: Parser Options
options = hsequence
    $ #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #config  <@=> strOption (long "config" <> short 'c' <> value ".dhall-ex.dhall" <> metavar "PATH" <> help "Configuration file")
   <: #only    <@=> option (pure <$> str) (long "only" <> value Nothing <> metavar "NAME" <> help "Exec subcommand only NAME in config")
   <: #subcmd  <@=> subcmdParser
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #sort     @= strArgument (metavar "PATH" <> help "file path") `withInfo` "Sort record keys in dhall file"
   <: #echo     @= strArgument (metavar "TEXT") `withInfo` "Echo TEXT"
   <: #init     @= pure () `withInfo` "Init dhall-ex work directory"
   <: #build    @= pure () `withInfo` "Build Dhall file to YAML or JSON"
   <: #deploy   @= deployCmdParser `withInfo` "Deploy builded config file to remote repository"
   <: #checkout @= checkoutCmdParser `withInfo` "Checkout repository in dhall workspace"
   <: #pull     @= pure () `withInfo` "Pull repository in dhall workspace"
   <: nil

deployCmdParser :: Parser Export.Deploy
deployCmdParser = hsequence
    $ #branch <@=> strOption (long "branch" <> short 'b' <> metavar "BRANCH" <> help "Checkout new branch to deploy")
   <: nil

checkoutCmdParser :: Parser Export.Checkout
checkoutCmdParser = hsequence
    $ #branch <@=> strArgument (metavar "BRANCH" <> help "Checkout branch")
   <: #new    <@=> switch (long "new" <> help "Checkout new branch")
   <: #reset  <@=> switch (long "reset" <> help "Checkout with Hard git reset")
   <: nil

variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k ((EmbedAt m . Field . pure) <$> getField x)

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc

version :: Parser (a -> a)
version = infoOption (Version.build Meta.version)
    $ long "version"
   <> help "Show version"
