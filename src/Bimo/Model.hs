{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Bimo.Model
    ( prettyName
    , checkModelConfigExist
    , readModelConfig
    , showModelConfig
    , createEmptyModel
    , copyModel
    , deleteModel
    , getModelPath
    , getModelLibPath
    , ModelException(..)
    ) where

import qualified Data.Text.IO as T
import Data.Yaml
import Data.Monoid
import Data.String
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

import Bimo.Types.Env
import Bimo.Types.Config.Model

import Bimo.Config
import Bimo.Path

prettyName :: (IsString a, Monoid a) => String -> String -> a
prettyName n c =
    let name = fromString n
        cat  = fromString c
     in cat `mappend` "/" `mappend` name

checkModelConfigExist :: (MonadIO m, MonadThrow m) => Path Abs File -> m ()
checkModelConfigExist p = unlessFileExists p $ throwM $ NotFoundModelConfig p

readModelConfig :: (MonadIO m, MonadThrow m)
                => Path Abs File
                -> m Model
readModelConfig p = do
    checkModelConfigExist p
    readYamlConfig p

showModelConfig :: (MonadIO m, MonadThrow m, MonadReader Env m, MonadLogger m)
                => String
                -> String
                -> m ()
showModelConfig n c = do
    path <- getModelPath n c
    colorized <- colorizedConfig $ fromAbsFile path
    logInfoN $ "Model config\n" <> colorized

getModelPath :: (MonadIO m, MonadThrow m, MonadReader Env m)
             => String
             -> String
             -> m (Path Abs File)
getModelPath n c = do
    mDir <- asks modelsDir
    mConf <- asks modelConfig
    name <- parseRelDir n
    cat <- parseRelDir c
    return $  mDir </> cat </> name </> mConf

createEmptyModel :: (MonadIO m, MonadThrow m, MonadReader Env m)
                 => String
                 -> Maybe String
                 -> Maybe String
                 -> Path Abs Dir
                 -> m ()
createEmptyModel name cat lang modelDir = do
    Env{..} <- ask
    createDir modelDir
    createDir $ modelDir </> modelSrc
    createDir $ modelDir </> modelExec
    let conf = emptyModelConfig name cat lang
    liftIO $ B.writeFile (toFilePath $ modelDir </> modelConfig) conf

copyModel :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader Env m)
          => Path Abs Dir
          -> Path Abs Dir
          -> m ()
copyModel src dst = do
    ensureDir dst
    copyDirRecur src dst

deleteModel :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader Env m)
            => Path Abs File
            -> m ()
deleteModel p = do
    let dir = parent p
        cat = parent dir
    removeDirRecur dir
    (ds, fs) <- listDir cat
    when (null ds && null fs) $ removeDir cat

getModelLibPath :: (MonadIO m, MonadThrow m, MonadReader Env m)
             => Path Abs Dir
             -> m (String, Path Abs Dir)
getModelLibPath pathToModelDir = do
    mConf     <- asks modelConfig
    mDir      <- asks modelsDir
    execDir   <- asks modelExec
    Model{..} <- readModelConfig $ pathToModelDir </> mConf
    exec      <- parseRelFile modelName
    name      <- parseRelDir modelName
    cat       <- parseRelDir category

    let execFile = pathToModelDir </> execDir </> exec
        dstDir   = mDir </> cat </> name

    whenDirExists dstDir $ throwM $ ModelAlreadyExists dstDir
    unlessFileExists execFile $ throwM $ NotFoundModelExec execFile

    return (category, dstDir)

data ModelException
    = NotFoundModelConfig !(Path Abs File)
    | NotFoundModelExec !(Path Abs File)
    | ModelAlreadyExists !(Path Abs Dir)

instance Exception ModelException

instance Show ModelException where
    show (NotFoundModelConfig path) =
        "Not found model config: " ++ show path
    show (NotFoundModelExec path) =
        "Not found model exec: " ++ show path
    show (ModelAlreadyExists path) =
        "Model with this name already exists in category: " ++ show path
