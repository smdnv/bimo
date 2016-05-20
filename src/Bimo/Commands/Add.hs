{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Add
    ( AddOpts(..)
    , add
    ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env

import Bimo.Model
import Bimo.Project
import Bimo.Path

data AddOpts
    = AddModel
    | AddTemplate
        { templateName :: !String }
    deriving Show

add :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => AddOpts
    -> m ()
add AddModel = do
    curDir <- getCurrentDir
    (_, dstDir) <- getModelLibPath curDir
    withAbsDir dstDir $ \dir -> copyModel curDir dir

add AddTemplate{..} = do
    tDir <- asks templatesDir
    name <- parseRelDir templateName
    let dst = tDir </> name

    src <- getCurrentDir
    withAbsDir dst $ \dir -> packProject src dir
