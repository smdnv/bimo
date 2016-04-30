{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Build model or project

module Bimo.Run
    ( run
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Process
import System.Exit

import Bimo.Types.Env
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config


run :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
    => m ()
run = do
    -- return ()
    pConf <- asks projectConfig
    p@Project{..} <- readProjectConfig pConf
    liftIO $ print p
    -- modelsDir <- asks projectModelsDir

