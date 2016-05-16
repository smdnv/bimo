{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module to interact with any app config

module Bimo.Config where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Yaml
import Path
import Path.IO
import System.Console.ANSI

import Bimo.Types.Env
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model


readYamlConfig :: (MonadIO m, MonadThrow m, FromJSON a)
               => Path Abs File
               -> m a
readYamlConfig p = do
    let file = fromAbsFile p
    decoded <- liftIO $ decodeFileEither file
    case decoded of
        Left e -> throwM e
        Right m -> return m

colorizedConfig :: (MonadIO m, MonadThrow m, MonadReader Env m, MonadLogger m)
                => FilePath
                -> m T.Text
colorizedConfig path = do
    content <- liftIO $ T.readFile path
    return $ T.unlines $ map insertSGRCode $ T.lines content
  where
    colorizedKey key =
        T.concat [ T.pack $ setSGRCode [SetColor Foreground Dull Blue]
                 , key
                 , T.pack $ setSGRCode []
                 ]
    insertSGRCode line =
        let (key, rest) = T.breakOn ":" line
         in if T.null rest
                then line
                else colorizedKey key <> rest

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


data ReadAppEnvException
    = NotFoundBuildScript !(Path Rel File)
    | LibraryDoesNotExist !(Path Abs Dir)

instance Exception ReadAppEnvException

instance Show ReadAppEnvException where
    show (NotFoundBuildScript path) =
        "Not found build script: " ++ show path
    show (LibraryDoesNotExist path) =
        "No library with name: " ++ show (dirname path) ++ ", path: " ++ show path



