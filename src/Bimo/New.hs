{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Create new project

module Bimo.New
    ( NewOpts (..)
    , new)
    where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import qualified Data.ByteString as B

import Bimo.Types.Env
import Bimo.Types.Config.Project

data NewOpts
    = NewProject  { projectName  :: String , srtTemplateName :: Maybe String }
    | NewModel    { modelName    :: String }
    | NewTemplate { templateName :: String }
    deriving Show

new :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
    => NewOpts
    -> m ()
new NewProject{..} = do
    dir <- parseRelDir projectName
    exists <- doesDirExist dir
    if exists
        then throwM $ AlreadyExists projectName
        else createEmptyProject dir
new NewModel{..} =
    liftIO $ print modelName
new NewTemplate{..} =
    liftIO $ print templateName

createEmptyProject :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                   => Path Rel Dir
                   -> m ()
createEmptyProject dir = do
    Env{..} <- ask
    createDir dir
    setCurrentDir dir
    createDir projectModelsDir
    liftIO $ B.writeFile (toFilePath projectConfig) emptyProjectConfig

data NewException
    = AlreadyExists !String
    deriving (Show)

instance Exception NewException


