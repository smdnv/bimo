{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Add
    ( AddOpts(..)
    , add
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import qualified Data.ByteString as B

import Bimo.Types.Env
import Bimo.Types.Config.Model
import Bimo.Types.Config.Project

import Bimo.Config

data AddOpts
    = AddModel
    | AddTemplate
        { templateName :: !String }
    deriving Show

add :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => AddOpts
    -> m ()
add AddModel = do
    curDir <- getCurrentDir
    (dstDir, execFile, _) <- getModelInfo curDir

    checkModelPaths dstDir execFile

    copyModel curDir dstDir

add AddTemplate{..} = do
    tDir     <- asks templatesDir
    tempName <- parseRelDir templateName
    let dstDir = tDir </> tempName

    exists <- doesDirExist dstDir
    when exists $ throwM $ TemplateAlreadyExists dstDir

    pConf       <- asks projectConfig
    mDir        <- asks projectModelsDir
    curDir      <- getCurrentDir
    Project{..} <- readProjectConfig $ curDir </> pConf
    case userModels of
        Nothing -> throwM NoUserModelsInConfig
        Just ms -> do
            modelsDirs  <- mapM (\(UserModel n _) -> do
                name <- parseRelDir n
                return $ curDir </> mDir </> name) ms
            modelsInfo <- mapM getModelInfo modelsDirs
            mapM_ (\(dst, exec, _) -> checkModelPaths dst exec) modelsInfo

            let lModels' = map (uncurry toLibModel) $
                    zipWith (\(_, _, cat) m -> (cat, m)) modelsInfo ms

                libModels' = Just $ maybe lModels' (++ lModels') libModels
                p = encodeProjectConfig $ Project Nothing libModels' topology

            mapM_ (uncurry copyModel) $
                    zipWith (\src (dst, _, _) -> (src, dst)) modelsDirs modelsInfo
            createDirIfMissing True dstDir
            liftIO $ B.writeFile (toFilePath $ dstDir </> pConf) p
  where
    toLibModel category (UserModel name args) = LibModel name category args

getModelInfo :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
              => Path Abs Dir
              -> m ((Path Abs Dir), (Path Abs File), String)
getModelInfo pathToModelDir = do
    mConf     <- asks modelConfig
    mDir      <- asks modelsDir
    execDir   <- asks modelExec
    Model{..} <- readModelConfig $ pathToModelDir </> mConf
    exec      <- parseRelFile modelName
    name      <- parseRelDir modelName
    cat       <- parseRelDir category

    let execFile = pathToModelDir </> execDir </> exec
        dstDir   = mDir </> cat </> name

    return (dstDir, execFile, category)

checkModelPaths :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
              => Path Abs Dir
              -> Path Abs File
              -> m ()
checkModelPaths dir exec = do
    exists <- doesDirExist dir
    when exists $ throwM $ ModelAlreadyExists dir

    exists <- doesFileExist exec
    unless exists $ throwM $ NotFoundModelExec exec

copyModel :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
          => Path Abs Dir
          -> Path Abs Dir
          -> m ()
copyModel src dst = do
    createDirIfMissing True dst
    copyDirRecur src dst


data AddException
    = NotFoundModelExec !(Path Abs File)
    | ModelAlreadyExists !(Path Abs Dir)
    | TemplateAlreadyExists !(Path Abs Dir)
    | NoUserModelsInConfig

instance Exception AddException

instance Show AddException where
    show (NotFoundModelExec path) =
        "Not found model exec: " ++ show path ++ " " ++ "Build model before add"
    show (ModelAlreadyExists path) =
        "Model with this name already exists in category: " ++ show path
    show (TemplateAlreadyExists path) =
        "Template with this name already exists: " ++ show path
    show NoUserModelsInConfig = "No user models in config"


