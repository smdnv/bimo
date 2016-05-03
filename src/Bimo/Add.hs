{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Add
    ( AddOpts(..)
    , add
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Config.Model

import Bimo.Config

data AddOpts
    = AddModel
    | AddTemplate
        { templateName :: !String }
    deriving Show

add :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => AddOpts
    -> m ()
add AddModel = do
    mConf     <- asks modelConfig
    mDir      <- asks modelsDir
    Model{..} <- readModelConfig mConf
    execDir   <- asks modelExec
    exec      <- parseRelFile modelName
    name      <- parseRelDir modelName
    cat       <- parseRelDir category

    let execFile = execDir </> exec
        dstDir   = mDir </> cat </> name

    exists <- doesFileExist execFile
    unless exists $ throwM $ NotFoundModelExec execFile

    exists <- doesDirExist dstDir
    when exists $ throwM $ ModelAlreadyExists dstDir

    createDirIfMissing True dstDir
    curDir <- getCurrentDir
    copyDirRecur curDir dstDir
add AddTemplate{..} = do
    liftIO $ print "add template"

data AddException
    = NotFoundModelExec !(Path Rel File)
   | ModelAlreadyExists !(Path Abs Dir)

instance Exception AddException

instance Show AddException where
    show (NotFoundModelExec path) =
        "Not found model exec: " ++ show path ++ " " ++ "Build model before add"
    show (ModelAlreadyExists path) =
        "Model with this name already exists in category: " ++ show path


