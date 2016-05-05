{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Unpack
    ( UnpackOpts(..)
    , unpack
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

import Bimo.Model

data UnpackOpts = UnpackOpts
    { modelCat :: !String
    , modelName :: !String
    } deriving Show

unpack :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
     => UnpackOpts
     -> m ()
unpack UnpackOpts{..} = do
    mDir <- asks modelsDir
    curDir <- getCurrentDir
    dir <- parseRelDir modelName
    cat <- parseRelDir modelCat
    let srcDir = mDir </> cat </> dir

    exists <- doesDirExist dir
    when exists $ throwM $ DirAlreadyExists dir

    copyModel srcDir $ curDir </> dir


data UnpackException
    = DirAlreadyExists !(Path Rel Dir)

instance Exception UnpackException

instance Show UnpackException where
    show (DirAlreadyExists path) =
        "Directory already exists: " ++ show path


