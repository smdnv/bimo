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
                  , unpackFlag   :: !Bool
                  }
    | NewModel    { modelName :: !String
                  , modelCat  :: !(Maybe String)
                  , modelLang :: !(Maybe String)
                  }
    deriving Show

new :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => NewOpts
    -> m ()
new NewProject{..} = do
    dir <- parseRelDir projectName
    curDir <- getCurrentDir
    let root = curDir </> dir

    checkExists root
    case (templateName, unpackFlag) of
        (Nothing, False) -> createEmptyProject root
        (Nothing, True) -> throwM NotProvidedTemplate
        (Just t, False) -> do
            createProjectDirs root
            copyProjectConfig t root
        (Just t, True) -> do
            createProjectDirs root
            unpackProject t root

new NewModel{..} = do
    dir <- parseRelDir modelName
    curDir <- getCurrentDir
    checkExists $ curDir </> dir
    createEmptyModel modelCat modelLang dir

checkExists :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
            => Path Abs Dir
            -> m ()
checkExists dir = do
    exists <- doesDirExist dir
    when exists $ throwM $ DirAlreadyExists dir


data NewException
    = DirAlreadyExists !(Path Abs Dir)
    | NotProvidedTemplate

instance Exception NewException

instance Show NewException where
    show (DirAlreadyExists path) =
        "Directory already exists: " ++ show path
    show NotProvidedTemplate =
        "Not provided template to unpack"


