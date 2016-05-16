{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new project

module Bimo.Commands.New
    ( NewOpts (..)
    , TemplateOpts (..)
    , new
    ) where

import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Config.Project

import Bimo.Config
import Bimo.Model
import Bimo.Project
import Bimo.Path

data NewOpts
    = NewProject  { projectName  :: !String
                  , templateOpts :: !(Maybe TemplateOpts)
                  }
    | NewModel    { modelName :: !String
                  , modelCat  :: !(Maybe String)
                  , modelLang :: !(Maybe String)
                  }
    deriving Show

data TemplateOpts = TemplateOpts
    { templateName :: !String
    , unpackFlag :: !Bool
    } deriving Show

new :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => NewOpts
    -> m ()
new NewModel{..} =
    withDir modelName $ \root -> do
        logInfoN $ T.concat [ "Create new model \""
                            , T.pack modelName
                            , "\""
                            ]
        createEmptyModel modelName modelCat modelLang root
new NewProject{..} =
    withDir projectName $ \root -> do
        logInfoN $ T.concat [ "Create new project\""
                            , T.pack projectName
                            , "\""
                            ]
        case templateOpts of
            Nothing -> createEmptyProject root
            Just (TemplateOpts temp False) -> do
                pConf <- asks projectConfig
                src <- getTemplatePath temp
                let dst = root </> pConf
                createProjectDirs root
                copyProjectConfig src dst
            Just (TemplateOpts temp True) -> do
                createProjectDirs root
                unpackProject temp root

