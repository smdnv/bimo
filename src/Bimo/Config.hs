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
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model


readYamlConfig :: (MonadIO m, MonadThrow m, MonadLogger m, FromJSON a)
               => Path Abs File
               -> m a
readYamlConfig p = do
    let file = fromAbsFile p
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
            -> m [Path Abs Dir]
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
        return path

getTemplatePath :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                => String
                -> m (Path Abs File)
getTemplatePath temp = do
    tDir <- asks templatesDir
    pConf <- asks projectConfig
    template <- parseRelDir temp
    let path = tDir </> template </> pConf

    exists <- doesFileExist path
    unless exists $ throwM $ NotFoundTemplate path
    return path


data ReadAppEnvException
    = NotFoundBuildScript !(Path Rel File)
    | LibraryDoesNotExist !(Path Abs Dir)
    | NotFoundTemplate !(Path Abs File)

instance Exception ReadAppEnvException

instance Show ReadAppEnvException where
    show (NotFoundBuildScript path) =
        "Not found build script: " ++ show path
    show (LibraryDoesNotExist path) =
        "No library with name: " ++ show (dirname path) ++ ", path: " ++ show path
    show (NotFoundTemplate path) =
        "Not found template: " ++ show path



