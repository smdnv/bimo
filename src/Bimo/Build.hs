{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Build model or project

module Bimo.Build
    ( BuildOpts (..)
    , build
    ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)
import qualified Data.ByteString as B

import Bimo.Types.Env
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config

data BuildOpts
    = BuildProject
    | BuildModel
    deriving Show

build :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
      => BuildOpts
      -> m ()
-- build BuildProject = do
build BuildModel = do
    Env{..} <- ask
    exists <- doesFileExist modelConfig
    unless exists $ throwM $ NotFoundModelConfig modelConfig
    m <- obtainModelConfig modelConfig
    liftIO $ print m

    -- read config
    -- read build script
    -- find required script
    -- build

data BuildException
    = NotFoundModelConfig !(Path Rel File)
    deriving (Show)

instance Exception BuildException
