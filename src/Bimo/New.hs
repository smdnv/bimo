{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Create new project

module Bimo.New
    ( NewOpts (..)
    , new
    ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)
import qualified Data.ByteString as B

import Bimo.Types.Env
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

data NewOpts
    = NewProject  { projectName     :: !String
                  , srcTemplateName :: !(Maybe String)
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
    checkExists dir createEmptyProject
new NewModel{..} = do
    dir <- parseRelDir modelName
    checkExists dir (createEmptyModel modelCat modelLang)

checkExists :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
            => Path Rel Dir
            -> (Path Rel Dir -> m ())
            -> m ()
checkExists dir action = do
    exists <- doesDirExist dir
    if exists
       then throwM $ AlreadyExists dir
       else action dir

createEmptyProject :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                   => Path Rel Dir
                   -> m ()
createEmptyProject dir = do
    Env{..} <- ask
    createDir dir
    createDir $ dir </> projectModelsDir
    liftIO $ B.writeFile (toFilePath $ dir </> projectConfig) emptyProjectConfig

createEmptyModel :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                 => Maybe String
                 -> Maybe String
                 -> Path Rel Dir
                 -> m ()
createEmptyModel cat lang modelDir = do
    Env{..} <- ask
    createDir modelDir
    createDir $ modelDir </> modelSrc
    createDir $ modelDir </> modelExec
    let name = dropTrailingPathSeparator $ toFilePath modelDir
        conf = emptyModelConfig name cat lang
    liftIO $ B.writeFile (toFilePath $ modelDir </> modelConfig) conf


data NewException
    = AlreadyExists !(Path Rel Dir)
    deriving (Show)

instance Exception NewException


