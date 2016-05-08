{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Delete where

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

data DeleteOpts
    = DeleteModel { modelName   :: !String
                  , modelCat    :: !String
                  , forceFlag   :: !String
                  }
    | DeleteTemplate { templateName :: !String
                     , templateFlag :: !TemplateFlag
                     }
    deriving Show

data TemplateFlag = Normal | Skip | Force
    deriving Show

delete :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => DeleteOpts
    -> m ()
delete DeleteTemplate{..} = do
    deleteTemplate templateName

