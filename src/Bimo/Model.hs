{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bimo.Model where

import Data.Yaml
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

readModelConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                => Path Abs File
                -> m Model
readModelConfig p = do
    exists <- doesFileExist p
    unless exists $ throwM $ NotFoundModelConfig p
    readYamlConfig p

createEmptyModel :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                 => Maybe String
                 -> Maybe String
                 -> Path Rel Dir
                 -> m ()
createEmptyModel cat lang modelDir = do
    Env{..} <- ask
    createDir modelDir
    createDir $ modelDir </> modelSrc
    createDir $ modelDir </> modelExec
    let name = dropTrailingPathSeparator $ toFilePath modelDir
        conf = emptyModelConfig name cat lang
    liftIO $ B.writeFile (toFilePath $ modelDir </> modelConfig) conf

copyModel :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
          => Path Abs Dir
          -> Path Abs Dir
          -> m ()
copyModel src dst = do
    ensureDir dst
    copyDirRecur src dst


data ModelException
    = NotFoundModelConfig !(Path Abs File)

instance Exception ModelException

instance Show ModelException where
    show (NotFoundModelConfig path) =
        "Not found model config: " ++ show path
