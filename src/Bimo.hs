{-# LANGUAGE RecordWildCards #-}
module Bimo
    (run)
    where

import Options.Applicative
import Control.Monad.Reader
import Control.Monad.Logger
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Commands
import Bimo.New
import Bimo.Build

run :: IO ()
run = do
    appDir           <- getAppUserDataDir "bimo"
    projectConfig    <- parseRelFile      "config.yaml"
    projectModelsDir <- parseRelDir       "models"
    modelSrc         <- parseRelDir       "src"
    modelExec        <- parseRelDir       "exec"
    modelConfig      <- parseRelFile      "model.yaml"
    modelsDir        <- parseRelDir       "models"
    modelsConfig     <- parseRelFile      "models.yaml"
    templatesDir     <- parseRelDir       "templates"
    let env = Env{..}

    args <- execParser parser
    case args of
        New opts -> runStdoutLoggingT $ runReaderT (new opts) env
        Build opts -> runStdoutLoggingT $ runReaderT (build opts) env
        _ -> print args


