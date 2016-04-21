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
    unless exists $ throwM $ NotFoundBuildScript scriptName
    return path

getLibPaths :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
            => String
            -> [String]
            -> m [FilePath]
getLibPaths lang libs = do
    langDir <- parseRelDir lang
    dir <- asks libsDir
    let prefix = dir </> langDir

    mapM (processPath prefix) libs
  where
    processPath prefix p = do
        libDir <- parseRelDir p
        let path = prefix </> libDir
        exists <- doesDirExist path
        unless exists $ throwM $ LibraryDoesNotExist path
        return $ fromAbsDir path


data ReadAppEnvironment
    = NotFoundBuildScript !(Path Rel File)
    | LibraryDoesNotExist !(Path Abs Dir)
    deriving (Show)

instance Exception ReadAppEnvironment



