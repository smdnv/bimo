{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bimo
    (bimo)
    where

import Options.Applicative
import Control.Monad.Reader
import Control.Monad.Logger
import Path
import Path.IO
import System.Environment

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
    -- appDataDir <- getAppUserDataDir "bimo"
    appDataDir <- getEnv "BIMO_DATA" >>= parseAbsDir
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
        env              = Env{..}

    args <- execParser parser
    let action :: ReaderT Env (LoggingT IO) () = case args of
            New    opts -> new opts
            Build  opts -> build opts
            Run         -> run
            Add    opts -> add opts
            Delete opts -> delete' opts
            Rename opts -> rename opts
            Unpack opts -> unpack opts
            List   opts -> list opts
            Show   opts -> show' opts
            _           -> fail "no command"

    runStdoutLoggingT $ runReaderT action env


