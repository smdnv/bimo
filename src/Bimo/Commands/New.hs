{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Create new project

module Bimo.Commands.New
    ( NewOpts (..)
    , new
    ) where

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

data NewOpts
    = NewProject  { projectName  :: !String
                  , templateName :: !(Maybe String)
                  }
    | NewModel    { modelName :: !String
                  , modelCat  :: !(Maybe String)
                  , modelLang :: !(Maybe String)
                  }
    deriving Show

new :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
    => NewOpts
    -> m ()
new NewProject{..} = do
    dir <- parseRelDir projectName
    checkExists dir
    case templateName of
        Nothing -> createEmptyProject dir
        Just template -> do
            pConf <- asks projectConfig
            tPath <- getTemplatePath template
            createDir dir
            copyFile tPath $ dir </> pConf

new NewModel{..} = do
    dir <- parseRelDir modelName
    checkExists dir
    createEmptyModel modelCat modelLang dir

checkExists :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
            => Path Rel Dir
            -> m ()
checkExists dir = do
    exists <- doesDirExist dir
    when exists $ throwM $ DirAlreadyExists dir


data NewException
    = DirAlreadyExists !(Path Rel Dir)

instance Exception NewException

instance Show NewException where
    show (DirAlreadyExists path) =
        "Directory already exists: " ++ show path


