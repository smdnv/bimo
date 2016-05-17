{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bimo
    (bimo)
    where

import Options.Applicative (execParser)
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Reader
import Control.Monad.Logger
import Path
import Path.IO
import System.Environment
import System.Console.ANSI
import System.Log.FastLogger (fromLogStr)

import Bimo.Types.Env
import Bimo.Commands
import Bimo.Commands.New
import Bimo.Commands.Build
import Bimo.Commands.Run
import Bimo.Commands.Add
import Bimo.Commands.Delete
import Bimo.Commands.Rename
import Bimo.Commands.Unpack
import Bimo.Commands.List
import Bimo.Commands.Show

bimo :: IO ()
bimo = do
    dataDir <- getEnv "BIMO_DATA" >>= parseAbsDir
    args    <- execParser parser
    -- runStdoutLoggingT $ runReaderT (action args) (env dataDir)
    runLoggingT (runReaderT (action args) (env dataDir)) colorizedLog

env :: Path Abs Dir -> Env
env appDataDir =
    let appDir           = appDataDir
        projectConfig    = $(mkRelFile "config.yaml")
        projectModelsDir = $(mkRelDir "models")
        modelSrc         = $(mkRelDir "src")
        modelExec        = $(mkRelDir "exec")
        modelConfig      = $(mkRelFile "model.yaml")
        modelsDir        = appDataDir </> $(mkRelDir "models")
        templatesDir     = appDataDir </> $(mkRelDir "templates")
        buildScriptsDir  = appDataDir </> $(mkRelDir "buildscripts")
        libsDir          = appDataDir </> $(mkRelDir "libs")
        runDir           = "bimo_run"
     in Env{..}

action :: Command -> ReaderT Env (LoggingT IO) ()
action args =
    case args of
        New    opts -> new opts
        Build  opts -> build opts
        Run    opts -> run opts
        Add    opts -> add opts
        Delete opts -> delete' opts
        Rename opts -> rename opts
        Unpack opts -> unpack opts
        List   opts -> list opts
        Show   opts -> show' opts
        _           -> fail "no command"

colorizedLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
colorizedLog loc src level msg = do
    let log = S8.concat [ "["
                        , colorizedLevel level
                        , "] "
                        , fromLogStr msg
                        ]
    S8.putStrLn log
  where
    colorizedLevel lev =
        let l = S8.pack $ drop 5 $ show lev
         in case lev of
                LevelInfo -> infoColor l
                LevelWarn -> warnColor l
                LevelError -> errorColor l
                _ -> l
    infoColor bs =
        S8.concat [ S8.pack $ setSGRCode [SetColor Foreground Dull Green]
                  , bs
                  , S8.pack $ setSGRCode []
                  ]
    warnColor bs =
        S8.concat [ S8.pack $ setSGRCode [SetColor Foreground Dull Yellow]
                  , bs
                  , S8.pack $ setSGRCode []
                  ]
    errorColor bs =
        S8.concat [ S8.pack $ setSGRCode [SetColor Foreground Dull Red]
                  , bs
                  , S8.pack $ setSGRCode []
                  ]



