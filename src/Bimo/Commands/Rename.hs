{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add model and template

module Bimo.Commands.Rename where

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

data RenameOpts
    = RenameModel { oldModelName :: !String
                  , oldModelCat  :: !String
                  , newModelName :: !String
                  , newModelCat  :: !String
                  , updateFlag   :: !Bool
                  }
    | RenameTemplate { oldTemplateName :: !String
                     , newTemplateName :: !String
                     }
    deriving Show

rename :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader Env m)
       => RenameOpts
       -> m ()
rename RenameTemplate{..} = do
    old <- getTemplatePath oldTemplateName
    new <- getTemplatePath newTemplateName
    let msg = T.concat [ "Rename template from \""
                       , prettyTemplateName old
                       , "\" to \""
                       , prettyTemplateName new
                       , "\" (yes/no)?"
                       ]

    userConfirm (logInfoN msg)
                (do copyProjectConfig old new
                    deleteTemplate old)
rename RenameModel{..} = do
    src <- getModelPath oldModelName oldModelCat
    dst <- getModelPath newModelName newModelCat
    checkModelConfigExist src
    whenFileExists dst $ throwM $ ModelAlreadyExists (parent dst)

    paths <- getTemplatesList
    ts <- getDependentTemplates oldModelName oldModelCat paths
    case (null ts, updateFlag) of
        (True, _) -> do
            let msg = T.concat [ "Rename model from \""
                               , prettyName oldModelName oldModelCat
                               , "\" to \""
                               , prettyName newModelName newModelCat
                               , "\" (yes/no)? "
                               ]
            userConfirm (logInfoN msg)
                        (do copyModel (parent src) (parent dst)
                            deleteModel src)
        (False, False) ->
            throwM $ ExistingDependence (prettyName oldModelName oldModelCat) ts
        (False, True) -> do
            let msg = T.concat [ "Rename model from \""
                               , prettyName oldModelName oldModelCat
                               , "\" to \""
                               , prettyName newModelName newModelCat
                               , "\nAnd update templates:\n"
                               , prettyTemplatesList " - " ts
                               , "\n(yes/no)? "
                               ]

            userConfirm (logWarnN msg)
                        (do mapM_ (updateModel oldModelName
                                               oldModelCat
                                               newModelName
                                               newModelCat) ts
                            copyModel (parent src) (parent dst)
                            deleteModel src)

