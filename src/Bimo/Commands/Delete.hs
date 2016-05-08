{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Delete where

import Data.List (foldl')
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

delete :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
    => DeleteOpts
    -> m ()
delete DeleteModel{..} = do
    templates <- getDependentTemplates modelName modelCat
    if null templates
        then do
            let msg = concat [ "Delete model: "
                             , modelCat
                             , "/"
                             , modelName
                             , "? (yes/no)"
                             ]
            userConfirm msg $ deleteModel modelName modelCat
        else
            case modelFlag of
                Normal -> throwM $
                    ExistingDependence (modelCat ++ "/" ++ modelName) templates
                Force -> do
                    let msg = concat [ "Model: "
                                     , modelCat
                                     , "/"
                                     , modelName
                                     , " used in these templates:\n"
                                     , show templates
                                     , "\nStill delete? "
                                     , "(yes/no)"
                                     ]
                    userConfirm msg $ deleteModel modelName modelCat

delete DeleteTemplate{..} =
    case templateFlag of
        Normal -> do
            let msg = concat [ "Delete template: "
                            , templateName
                            , "? (yes/no)"
                            ]

            userConfirm msg $ deleteTemplate templateName
        Skip -> do
            tPath <- getTemplatePath templateName
            Project{..} <- readProjectConfig tPath
            case libModels of
                Nothing -> throwM $ NoLibModelsInConfig tPath
                Just models -> do
                    (toDelete, toSkip) <- foldl' process (return ([], [])) models
                    liftIO $ print toDelete
                    liftIO $ print toSkip
  where
    process acc (LibModel n c _) = do
        (toDelete, toSkip) <- acc
        templates <- getDependentTemplates n c
        return $ if null templates
             then ((n, c, templates):toDelete, toSkip)
             else (toDelete, (n, c, templates):toSkip)

    deleteModel (LibModel n c _) = catch (delete $ DeleteModel n c Normal) skip
    skip (ExistingDependence n _) = liftIO $ putStrLn $ "skip model: " ++ n
    skip e = throwM e

userConfirm msg yes = do
    liftIO $ putStrLn msg
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

