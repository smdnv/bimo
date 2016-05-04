{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Build model or project

module Bimo.Commands.Build
    ( BuildOpts (..)
    , build
    ) where

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Process
import System.Exit

import Bimo.Types.Env
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config

data BuildOpts
    = BuildProject
    | BuildModel
    deriving Show

build :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m)
      => BuildOpts
      -> m ()
build BuildProject = do
    pConf <- asks projectConfig
    curDir <- getCurrentDir
    p@Project{..} <- readProjectConfig $ curDir </> pConf
    modelsDir <- asks projectModelsDir

    maybe (return ()) (buildModels modelsDir) userModels
  where
    buildModels root models =
        mapM_ (\(UserModel n _) -> do
            name <- parseRelDir n
            let dir = root </> name
            withCurrentDir dir $ build BuildModel) models

build BuildModel = do
    mConf       <- asks modelConfig
    curDir      <- getCurrentDir
    Model{..}   <- readModelConfig $ curDir </> mConf

    name        <- parseRelFile modelName
    execDir     <- asks modelExec
    srcDir      <- asks modelSrc
    buildScript <- getBuildScript language
    libPaths    <- getLibPaths language libs
    files       <- mapM (\p -> do path <- parseRelFile p
                                  return $ srcDir </> path) srcFiles

    let execFile = execDir </> name

    require <- requireBuild files execFile
    when require $ build' buildScript files execFile libPaths
  where
    requireBuild src dst = do
        exists <- doesFileExist dst
        if exists
            then do times <- mapM getModificationTime src
                    dstTime <- getModificationTime dst
                    return $ dstTime <= maximum times
            else return True
    build' script src dst libs = do
        let libs' = map fromAbsDir libs
            src' = map fromRelFile src
            args = [ "-s"
                   , foldl' (++) "" $ intersperse ":" src'
                   , "-d"
                   , fromRelFile dst
                   ] ++ if null libs then [] else
                   [ "-l"
                   , foldl' (++) "" $ intersperse ":" (map fromAbsDir libs)
                   ]
            p = proc (fromAbsFile script) args

        liftIO $ mapM_ print args

        (ec, out, err) <- liftIO $ readCreateProcessWithExitCode p ""
        unless (ec == ExitSuccess) $ throwM $ ModelBuildFailure dst out err


data BuildException
    = ModelBuildFailure !(Path Rel File) !String !String

instance Exception BuildException

instance Show BuildException where
    show (ModelBuildFailure name out err) = concat
        [ "Failure when build model exec: " ++ show name ++ "\n"
        , "stdout: \n" ++ out ++ "\n"
        , "stderr: \n" ++ err ++ "\n"
        ]
