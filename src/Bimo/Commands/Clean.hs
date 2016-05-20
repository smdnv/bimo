{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new project

module Bimo.Commands.Clean
    ( clean
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
import Bimo.Project
import Bimo.Path


clean :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
      => m ()
clean = do
    curDir    <- getCurrentDir
    pConf     <- asks projectConfig
    modelsDir <- asks projectModelsDir
    execDir   <- asks modelExec

    Project{..} <- readProjectConfig $ curDir </> pConf

    case userModels of
        Nothing -> logInfoN "Nothing to clean, project contents only libs models"
        Just ms -> do
            logInfoN "Clean project"
            mapM_ (\(UserModel n _ _ _ _) -> do
                exec  <- parseRelFile n
                model <- parseRelDir n
                let execPath = curDir </> modelsDir
                                      </> model
                                      </> execDir
                                      </> exec

                removeIfExist execPath) ms
  where
    removeIfExist p = do
        exists <- doesFileExist p
        when exists $ removeFile p
