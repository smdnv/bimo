{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Bimo.New
import Bimo.Build
import Bimo.Run

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
        modelsConfig     = appDataDir </> $(mkRelFile "models.yaml")
        templatesDir     = appDataDir </> $(mkRelDir "templates")
        buildScriptsDir  = appDataDir </> $(mkRelDir "buildscripts")
        libsDir          = appDataDir </> $(mkRelDir "libs")
        env              = Env{..}

    args <- execParser parser
    case args of
        New opts -> runStdoutLoggingT $ runReaderT (new opts) env
        Build opts -> runStdoutLoggingT $ runReaderT (build opts) env
        Run -> runStdoutLoggingT $ runReaderT run env
        _ -> print args


