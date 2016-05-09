{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Rename where

import qualified Data.Text as T
import Data.List
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Config.Project

import Bimo.Model
import Bimo.Project
import Bimo.Path

data RenameOpts
    = RenameModel { oldModelName :: !String
                  , oldModelCat  :: !String
                  , newModelName :: !String
                  , newModelCat  :: !String
                  , recurUpdate  :: !Bool
                  }
    | RenameTemplate { oldTemplateName :: !String
                     , newTemplateName :: !String
                     }
    deriving Show

rename :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
       => RenameOpts
       -> m ()
rename RenameTemplate{..} = do
    old <- getTemplatePath oldTemplateName
    new <- getTemplatePath newTemplateName

    copyProjectConfig old new
    deleteTemplate old



