{-# LANGUAGE FlexibleContexts #-}
-- | Module to interact with any app config

module Bimo.Config where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Yaml
import Path
import Path.IO

import Bimo.Types.Env
-- import Bimo.Types.Config.Project
import Bimo.Types.Config.Model


readModelConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                => Path Rel File
                -> m Model
readModelConfig p = do
    let file = fromRelFile p
    decoded <- liftIO $ decodeFileEither file
    case decoded of
        Left e -> throwM e
        Right m -> return m

getBuildScript :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
               => String
               -> m (Path Abs File)
getBuildScript lang = do
    scriptName <- parseRelFile lang
    scriptsDir <- asks buildScriptsDir
    let path = scriptsDir </> scriptName

    exists <- doesFileExist path
    if exists
       then return path
       else throwM $ NotFoundBuildScript scriptName

data ReadAppEnvironment
    = NotFoundBuildScript !(Path Rel File)
    deriving (Show)

instance Exception ReadAppEnvironment



