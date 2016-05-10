{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bimo.Project
    ( prettyTemplateName
    , prettyTemplatesList
    , checkProjectConfigExist
    , readProjectConfig
    , writeProjectConfig
    , showProjectConfig
    , createProjectDirs
    , createEmptyProject
    , copyProjectConfig
    , deleteTemplate
    , updateModel
    , updateProjectWith
    , getTemplatePath
    , getTemplatesList
    , getDependentTemplates
    , unpackProject
    , packProject
    , fillModels
    , ProjectException(..)
    ) where

import Data.Yaml
import Data.String
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
import System.FilePath (dropTrailingPathSeparator)

import Bimo.Types.Env
import Bimo.Types.Project
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config
import Bimo.Model
import Bimo.Path

prettyTemplateName :: (Monoid a, IsString a) => Path Abs File -> a
prettyTemplateName =
    fromString . dropTrailingPathSeparator . fromRelDir . dirname . parent

prettyTemplatesList :: (Monoid a, IsString a) => [Path Abs File] -> a
prettyTemplatesList ts =
    foldl' mappend mempty $ map func ts
  where
    func = fromString . dropTrailingPathSeparator . fromRelDir . dirname . parent

checkProjectConfigExist :: (MonadIO m, MonadThrow m) => Path Abs File -> m ()
checkProjectConfigExist p = unlessFileExists p $ throwM $ NotFoundProjectConfig p

readProjectConfig :: (MonadIO m, MonadThrow m)
                  => Path Abs File
                  -> m Project
readProjectConfig p = do
    checkProjectConfigExist p
    readYamlConfig p

writeProjectConfig :: (MonadIO m, MonadThrow m, MonadReader Env m)
                   => Path Abs File
                   -> Project
                   -> m ()
writeProjectConfig path conf = do
    let b = encodeProjectConfig conf
    liftIO $ B.writeFile (toFilePath path) b

showProjectConfig :: (MonadIO m, MonadThrow m, MonadReader Env m)
                   => String
                   -> m ()
showProjectConfig t =
    getTemplatePath t >>= readProjectConfig >>= liftIO . print

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
    return $ tDir </> template </> pConf

getTemplatesList :: (MonadIO m, MonadThrow m, MonadReader Env m)
                => m [Path Abs File]
getTemplatesList = do
    tDir <- asks templatesDir
    pConf <- asks projectConfig
    content <- listDir tDir
    return $ map (</> pConf) $ fst content

getDependentTemplates :: (MonadIO m, MonadThrow m, MonadReader Env m)
                      => String
                      -> String
                      -> [Path Abs File]
                      -> m [Path Abs File]
getDependentTemplates n c templates = do
    p <- getModelPath n c
    checkModelConfigExist p
    filterM dependOn templates
  where
    dependOn template = do
        Project{..} <- readProjectConfig template
        case libModels of
            Nothing -> throwM $ NoLibModelsInConfig template
            Just models -> do
                let model = LibModel n c []
                return (model `elem` models)

copyProjectConfig :: (MonadIO m, MonadThrow m, MonadReader Env m)
                  => Path Abs File
                  -> Path Abs File
                  -> m ()
copyProjectConfig src dst = do
    checkProjectConfigExist src
    whenFileExists dst $ throwM $ TemplateAlreadyExists dst
    ensureDir $ parent dst
    copyFile src dst

deleteTemplate :: (MonadIO m, MonadThrow m, MonadReader Env m)
               => Path Abs File
               -> m ()
deleteTemplate t = do
    checkProjectConfigExist t
    removeFile t
    removeDir $ parent t

updateModel :: (MonadIO m, MonadThrow m, MonadReader Env m)
            => String
            -> String
            -> String
            -> String
            -> Path Abs File
            -> m ()
updateModel oldN oldC newN newC =
    updateProjectWith updateModel'
  where
    updateModel' Project{..} =
        let model = LibModel oldN oldC []
         in case libModels of
                Nothing -> Nothing
                Just models ->
                    case find (== model) models of
                        Nothing -> Nothing
                        Just m@(LibModel _ _ args) ->
                            let models' = LibModel newN newC args : delete m models
                             in return $ Project userModels (Just models') topology

updateProjectWith :: (MonadIO m, MonadThrow m, MonadReader Env m)
                  => (Project -> Maybe Project)
                  -> Path Abs File
                  -> m ()
updateProjectWith update p = do
    conf <- readProjectConfig p
    case update conf of
        Nothing -> throwM $ FailedUpdateProjectConfig p
        Just conf' -> writeProjectConfig p conf'


unpackProject :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader Env m)
              => String
              -> Path Abs Dir
              -> m ()
unpackProject temp root = do
    pConf    <- asks projectConfig
    mLibDir  <- asks modelsDir
    mPrjDir  <- asks projectModelsDir
    tempPath <- getTemplatePath temp
    checkProjectConfigExist tempPath
    let dstDir = root </> mPrjDir

    Project{..} <- readProjectConfig tempPath
    case libModels of
        Nothing -> throwM $ NoLibModelsInConfig tempPath
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

packProject :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader Env m)
            => String
            -> Path Abs Dir
            -> m ()
packProject temp root = do
    tDir     <- asks templatesDir
    tempName <- parseRelDir temp
    let dstDir = tDir </> tempName

    dstPath <- getTemplatePath temp

    whenFileExists dstPath $ throwM $ TemplateAlreadyExists dstPath

    pConf  <- asks projectConfig
    mDir   <- asks projectModelsDir
    curDir <- getCurrentDir
    let conf = curDir </> pConf
        srcDir = curDir </> mDir

    Project{..} <- readProjectConfig conf
    case userModels of
        Nothing -> throwM $ NoUserModelsInConfig conf
        Just ms -> do
            (models, srcDst) <- mapAndUnzipM (process srcDir) ms
            let lModels = Just $ maybe models (++ models) libModels
                prj = Project Nothing lModels topology

            mapM_ (uncurry copyModel) srcDst

            ensureDir dstDir
            writeProjectConfig dstPath prj
    where
      process srcRoot m@(UserModel name _) = do
          name' <- parseRelDir name
          let srcDir = srcRoot </> name'

          (cat, dstDir) <- getModelLibPath srcDir
          let lModel = toLibModel cat m

          return (lModel, (srcDir, dstDir))


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
    | TemplateAlreadyExists !(Path Abs File)
    | NotFoundAnyModel ![String]
    | NotFoundModelInConfig !String
    | NoLibModelsInConfig !(Path Abs File)
    | NoUserModelsInConfig !(Path Abs File)
    | FailedUpdateProjectConfig !(Path Abs File)
    | ExistingDependence !String ![Path Abs File]

instance Exception ProjectException

instance Show ProjectException where
    show (NotFoundProjectConfig path) =
        "Not found project config: " ++ show path
    show (TemplateAlreadyExists path) =
        "Template with this name already exists: " ++ show path
    show (NotFoundAnyModel ms) =
        "Not found any model in config file: " ++ show ms
    show (NotFoundModelInConfig name) =
        "Not found model in config: " ++ show name
    show (NoLibModelsInConfig path) =
        "Not found any lib model in config: " ++ show path
    show (NoUserModelsInConfig path) =
        "Not found any user model in config: " ++ show path
    show (FailedUpdateProjectConfig path) =
        "Failed update project config: " ++ show path
    show (ExistingDependence model templates) =
        "Existing dependence with: " ++ show templates



