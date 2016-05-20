{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Delete where

import qualified Data.Text as T
import Data.List
import Data.Monoid
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
    case (null ts, f) of
        (True, _) -> do
            let msg = T.concat [ "Delete model: "
                               , prettyName n c
                               , "? (yes/no)"
                               ]
            userConfirm (logInfoN msg)
                        (getModelPath n c >>= deleteModel)
        (False, Normal) ->
            throwM $ ExistingDependence (prettyName n c) ts
        (False, Force) -> do
            let msg = T.concat [ "Delete model: "
                               , prettyName n c
                               , "\nDependent templates, break if delete model\n"
                               , prettyTemplatesList " - " ts
                               , "\nAnyway delete? (yes/no):"
                               ]
            userConfirm (logWarnN msg)
                        (getModelPath n c >>= deleteModel)

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
                    (d, s, nf) <- foldl' (process tPath)
                                     (return ([], [], []))
                                     models
                    case f' of
                        Skip ->
                            userConfirm (logInfoN $ deleteTempSkipMsg s d t)
                                        (do mapM_ (\(n, c, _) -> getModelPath n c >>= deleteModel) d
                                            deleteTemplate tPath)
                        Force -> do
                            let ts = d ++ s
                            userConfirm (logWarnN $ deleteTempForceMsg s d t)
                                        (do mapM_ (\(n, c, _) -> getModelPath n c >>= deleteModel) ts
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
                 , "Delete models:\n"
                 , prettyDelete d
                 , "Delete template: "
                 , T.pack t
                 , "? (yes/no)"
                 ]
    deleteTempForceMsg s d t =
        T.concat [ "Delete models used in other templates:\n"
                 , prettyDependency s
                 , "Delete models:\n"
                 , prettyDelete d
                 , "Delete template: "
                 , T.pack t
                 , "? (yes/no)"
                 ]
    prettyDelete = T.concat . map (\(n, c, _) -> " - " <> prettyName n c <> "\n")
    prettyDependency =
        T.concat . map (\(n, c, ts) ->
            T.concat [ "\""
                     , prettyName n c
                     , "\" used in templates:\n"
                     , prettyTemplatesList " - " ts
                     , "\n"
                     ])
    process path acc (LibModel n c _ _ _ _) = do
        (toDelete, toSkip, notFound) <- acc
        paths <- getTemplatesList
        let paths' = delete path paths

        templates <- tryJust predicate $ getDependentTemplates n c paths'
        return $ case templates of
            Left p -> (toDelete, toSkip, (n, c):notFound)
            Right ts -> if null ts
                           then ((n, c, ts):toDelete, toSkip, notFound)
                           else (toDelete, (n, c, ts):toSkip, notFound)
    predicate (NotFoundModelConfig p) = Just p
    predicate _ = Nothing
