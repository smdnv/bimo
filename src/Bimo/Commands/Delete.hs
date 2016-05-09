{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Delete where

import qualified Data.Text as T
import Data.List
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Types.Config.Project

import Bimo.Model
import Bimo.Project
import Bimo.Path

data DeleteOpts
    = DeleteModel { modelName :: !String
                  , modelCat  :: !String
                  , modelFlag :: !Flags
                  }
    | DeleteTemplate { templateName :: !String
                     , templateFlag :: !Flags
                     }
    deriving Show

data Flags = Normal | Skip | Force
    deriving Show

delete' :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
        => DeleteOpts
        -> m ()
delete' (DeleteModel n c f) = do
    paths <- getTemplatesList
    ts <- getDependentTemplates n c paths
    if null ts
        then userConfirm (logInfoN $ deleteModelNormalMsg n c)
                         (deleteModel n c)
        else
            case f of
                Normal -> throwM $
                    ExistingDependence (prettyName n c) ts
                Force -> userConfirm (logWarnN $ deleteModelForceMsg n c ts)
                                     (deleteModel n c)
  where
    deleteModelNormalMsg n c =
        T.concat [ "Delete model: "
                 , prettyName n c
                 , "? (yes/no)"
                 ]
    deleteModelForceMsg n c ts =
        T.concat [ "Delete model: "
                 , prettyName n c
                 , "\nDependent templates, break if delete model\n"
                 , prettyTemplatesList ts
                 , "\nAnyway delete? (yes/no):"
                 ]

delete' (DeleteTemplate t f) = do
    tPath <- getTemplatePath t
    case f of
        Normal -> userConfirm (logInfoN $ deleteTempNormalMsg t)
                              (deleteTemplate tPath)
        f'   -> do
            Project{..} <- readProjectConfig tPath
            case libModels of
                Nothing -> throwM $ NoLibModelsInConfig tPath
                Just models -> do
                    (d, s) <- foldl' (process tPath)
                                     (return ([], []))
                                     models
                    case f' of
                        Skip ->
                            userConfirm (logInfoN $ deleteTempSkipMsg s d t)
                                        (do mapM_ (\(n, c, _) -> deleteModel n c) d
                                            deleteTemplate tPath)
                        Force -> do
                            let ts = d ++ s
                            userConfirm (logWarnN $ deleteTempForceMsg s d t)
                                        (do mapM_ (\(n, c, _) -> deleteModel n c) ts
                                            deleteTemplate tPath)
  where
    deleteTempNormalMsg t =
        T.concat [ "Delete template: "
                 , T.pack t
                 , "? (yes/no)"
                 ]
    deleteTempSkipMsg s d t =
        T.concat [ "Skip models:\n"
                 , prettyDependency s
                 , "\nDelete models:\n"
                 , prettyDependency d
                 , "\nDelete template: "
                 , T.pack t
                 , "? (yes/no)"
                 ]
    deleteTempForceMsg s d t =
        T.concat [ "Delete models used in other templates:\n"
                 , prettyDependency s
                 , "\nDelete models:\n"
                 , prettyDependency d
                 , "\nDelete template: "
                 , T.pack t
                 , "? (yes/no)"
                 ]
    prettyDependency =
        T.concat . map (\(n, c, ts) -> T.concat [ prettyName n c
                                                , prettyTemplatesList ts
                                                , "\n"
                                                ])
    process path acc (LibModel n c _) = do
        (toDelete, toSkip) <- acc
        paths <- getTemplatesList
        let paths' = delete path paths

        templates <- getDependentTemplates n c paths'
        return $ if null templates
             then ((n, c, templates):toDelete, toSkip)
             else (toDelete, (n, c, templates):toSkip)

userConfirm :: (MonadIO m, MonadThrow m) => m () -> m () -> m ()
userConfirm log yes = do
    log
    line <- liftIO getLine
    case line of
        "yes"   -> yes
        "no"    -> liftIO $ putStrLn "Cancel command"
        invalid -> throwM $ AbortCommand invalid

data DeleteException
    = ExistingDependence !String ![Path Abs File]
    | AbortCommand !String

instance Exception DeleteException

instance Show DeleteException where
    show (ExistingDependence model paths) =
        "Existing dependence with: " ++ show paths
    show (AbortCommand input) =
        "Abort command, invalid input: " ++ show input

