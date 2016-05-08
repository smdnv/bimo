{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Show
    ( ShowOpts(..)
    , show'
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Config.Project

import Bimo.Project

data ShowOpts
    = ShowTemplate { templateName :: !String }
    | ShowModel    { modelName :: !String
                   , modelCat :: !String
                   }
    deriving Show

show' :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m)
      => ShowOpts
      -> m ()
show' ShowTemplate{..} = showProjectConfig templateName

