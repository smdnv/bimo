{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.List
    ( ListOpts(..)
    , list
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

data ListOpts
    = ListModels
    | ListTemplates
    deriving Show

list :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
     => ListOpts
     -> m ()
list ListModels = do
    mDir <- asks modelsDir
    files <- listDir mDir
    let categories = fst files

    liftIO $ mapM_ prettyPrint categories
  where
    prettyPrint category = do
        print category
        files <- listDir category
        mapM_ (\m -> putStrLn $ "  - " ++ show m) $ fst files

list ListTemplates = do
    tempDir <- asks templatesDir
    files <- listDir tempDir
    let ts = fst files

    liftIO $ mapM_ print ts


