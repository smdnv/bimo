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
import Bimo.Path

data UnpackOpts = UnpackOpts
    { modelCat :: !String
    , modelName :: !String
    } deriving Show

unpack :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
     => UnpackOpts
     -> m ()
unpack UnpackOpts{..} = do
    mDir <- asks modelsDir
    dir <- parseRelDir modelName
    cat <- parseRelDir modelCat
    let srcDir = mDir </> cat </> dir

    withRelDir modelName $ \root -> copyModel srcDir root


