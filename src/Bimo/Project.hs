{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bimo.Project
    ( readProjectConfig
    , writeProjectConfig
    , createProjectDirs
    , createEmptyProject
    , copyProjectConfig
    , unpackProject
    , fillModels
    ) where

import Data.Yaml
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Project
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config
import Bimo.Model

readProjectConfig :: (MonadIO m, MonadThrow m)
                  => Path Abs File
                  -> m Project
readProjectConfig p = do
    exists <- doesFileExist p
    unless exists $ throwM $ NotFoundProjectConfig p
    readYamlConfig p

writeProjectConfig :: (MonadIO m, MonadThrow m, MonadReader Env m)
                   => Path Abs File
                   -> Project
                   -> m ()
writeProjectConfig path conf = do
    let b = encodeProjectConfig conf
    liftIO $ B.writeFile (toFilePath path) b

createProjectDirs :: (MonadIO m, MonadThrow m, MonadReader Env m)
                  => Path Abs Dir
                  -> m ()
createProjectDirs root = do
    mDir <- asks projectModelsDir
    createDir root
    createDir $ root </> mDir

createEmptyProject :: (MonadIO m, MonadThrow m, MonadReader Env m)
                   => Path Abs Dir
                   -> m ()
createEmptyProject root = do
    pConf <- asks projectConfig
    createProjectDirs root
    writeProjectConfig (root </> pConf) emptyProjectConfig

getTemplatePath :: (MonadIO m, MonadThrow m, MonadReader Env m)
                => String
                -> m (Path Abs File)
getTemplatePath temp = do
    tDir <- asks templatesDir
    pConf <- asks projectConfig
    template <- parseRelDir temp
    let path = tDir </> template </> pConf

    exists <- doesFileExist path
    unless exists $ throwM $ NotFoundTemplate path
    return path

copyProjectConfig :: (MonadIO m, MonadThrow m, MonadReader Env m)
                  => String
                  -> Path Abs Dir
                  -> m ()
copyProjectConfig temp root = do
    pConf <- asks projectConfig
    srcPath <- getTemplatePath temp
    copyFile srcPath $ root </> pConf

unpackProject :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader Env m)
              => String
              -> Path Abs Dir
              -> m ()
unpackProject temp root = do
    pConf <- asks projectConfig
    mLibDir <- asks modelsDir
    mPrjDir <- asks projectModelsDir
    tempPath <- getTemplatePath temp
    let dstDir = root </> mPrjDir

    Project{..} <- readProjectConfig tempPath
    case libModels of
        Nothing -> throwM $ NotFoundLibModels tempPath
        Just ms -> do
            (models, srcDst) <- mapAndUnzipM (process mLibDir dstDir) ms
            let prj = Project (Just models) Nothing topology

            mapM_ (uncurry copyModel) srcDst

            writeProjectConfig (root </> pConf) prj
  where
    process srcRoot dstRoot m@(LibModel name cat _) = do
        let uModel = toUserModel m

        name' <- parseRelDir name
        cat' <- parseRelDir cat
        return (uModel, (srcRoot </> cat' </> name', dstRoot </> name'))

fillModels :: (MonadIO m, MonadThrow m, MonadReader Env m)
           => Project
           -> m (M.Map String ModelEntity)
fillModels (Project uModels lModels t) = do
    let tModels = modelsFromTopology t

    libRoot <- asks modelsDir
    projectRoot <- asks projectModelsDir
    execDir <- asks modelExec
    pModels <- case (uModels, lModels) of
        (Nothing, Nothing) -> throwM $
            NotFoundAnyModel $ M.foldlWithKey (\ks k _ -> k:ks) [] tModels
        (Just u, Nothing) -> return $ modelsToMap u
        (Nothing, Just l) -> return $ modelsToMap l
        (Just u, Just l)  -> return $ modelsToMap (u ++ l)

    fillModels' pModels tModels libRoot projectRoot execDir
  where
    modelsToMap = foldl' func M.empty
      where
        func acc m@(UserModel n _) = M.insert n m acc
        func acc m@(LibModel n _ _) = M.insert n m acc

    fillModels' pModels tModels libRoot projectRoot execDir =
        M.foldlWithKey func (return M.empty) tModels
      where
        func acc k ModelEntity{..} = do
            acc' <- acc
            model <- case M.lookup k pModels of
                Nothing -> throwM $ NotFoundModelInConfig k
                Just (UserModel _ execArgs) -> do
                    name <- parseRelDir k
                    exec <- parseRelFile k
                    path <- makeAbsolute $ projectRoot </> name
                                                       </> execDir
                                                       </> exec
                    let execPath = fromAbsFile path
                    exists <- doesFileExist path
                    unless exists $ throwM $ NotFoundModelExec path
                    return ModelEntity{..}
                Just (LibModel _ c execArgs) -> do
                    cat <- parseRelDir c
                    name <- parseRelDir k
                    exec <- parseRelFile k
                    let path = libRoot </> cat </> name </> execDir </> exec
                        execPath = fromAbsFile path
                    exists <- doesFileExist path
                    unless exists $ throwM $ NotFoundModelExec path
                    return ModelEntity{..}
            return $ M.insert k model acc'


data ProjectException
    = NotFoundProjectConfig !(Path Abs File)
    | NotFoundTemplate !(Path Abs File)
    | NotFoundAnyModel ![String]
    | NotFoundModelInConfig !String
    | NotFoundModelExec !(Path Abs File)
    | NotFoundLibModels !(Path Abs File)

instance Exception ProjectException

instance Show ProjectException where
    show (NotFoundProjectConfig path) =
        "Not found project config: " ++ show path
    show (NotFoundTemplate path) =
        "Not found template: " ++ show path
    show (NotFoundAnyModel ms) =
        "Not found any model in config file: " ++ show ms
    show (NotFoundModelInConfig name) =
        "Not found model in config: " ++ show name
    show (NotFoundModelExec path) =
        "Not found model exec: " ++ show path
    show (NotFoundLibModels path) =
        "Not found any lib model in config: " ++ show path



