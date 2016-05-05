{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bimo.Project
    ( readProjectConfig
    , createEmptyProject
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

readProjectConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                  => Path Abs File
                  -> m Project
readProjectConfig p = do
    exists <- doesFileExist p
    unless exists $ throwM $ NotFoundProjectConfig p
    readYamlConfig p

createEmptyProject :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
                   => Path Rel Dir
                   -> m ()
createEmptyProject dir = do
    Env{..} <- ask
    createDir dir
    createDir $ dir </> projectModelsDir
    liftIO $ B.writeFile (toFilePath $ dir </> projectConfig) emptyProjectConfig

fillModels :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
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
    | NotFoundAnyModel ![String]
    | NotFoundModelInConfig !String
    | NotFoundModelExec !(Path Abs File)

instance Exception ProjectException

instance Show ProjectException where
    show (NotFoundProjectConfig path) =
        "Not found project config: " ++ show path
    show (NotFoundAnyModel ms) =
        "Not found any model in config file: " ++ show ms
    show (NotFoundModelInConfig name) =
        "Not found model in config: " ++ show name
    show (NotFoundModelExec path) =
        "Not found model exec: " ++ show path



