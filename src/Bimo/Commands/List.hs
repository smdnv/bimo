{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.List
    ( ListOpts(..)
    , list
    ) where

import qualified Data.Text as T
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

import Bimo.Types.Env

import Bimo.Project

data ListOpts
    = ListModels
    | ListTemplates
    deriving Show

list :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Env m)
     => ListOpts
     -> m ()
list ListModels = do
    mDir <- asks modelsDir
    content <- listDir mDir
    let categories = fst content
        msg = "Available models:\n"

    if null categories
        then logInfoN $ msg <> "Empty"
        else do
            pairs <- mapM (\cat -> do
                catContent <- liftM fst $ listDir cat
                return (toText cat, map toText catContent)) categories

            logInfoN $ msg <> T.concat (map prettyPair pairs)
  where
    toText = T.pack . dropTrailingPathSeparator . fromRelDir . dirname
    prettyPair (cat, models) =
        "Category: " <> cat
                     <> "\n"
                     <> T.concat (map (\m -> " - " <>  m <> "\n") models)

list ListTemplates = do
    templates <- getTemplatesList
    let msg = "Available templates:\n"

    if null templates
        then logInfoN $ msg <> "Empty"
        else logInfoN $ T.concat [ "Available templates:\n"
                                 , prettyTemplatesList templates
                                 ]

