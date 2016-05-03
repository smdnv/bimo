{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Process models before run

module Bimo.Project
    ( fillModels
    ) where

import qualified Data.Map as M
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
                    return ModelEntity{..}
                Just (LibModel _ c execArgs) -> do
                    cat <- parseRelDir c
                    name <- parseRelDir k
                    exec <- parseRelFile k
                    let execPath = fromAbsFile $ libRoot </> cat
                                                         </> name
                                                         </> execDir
                                                         </> exec
                    return ModelEntity{..}
            return $ M.insert k model acc'


data ProjectException
    = NotFoundAnyModel ![String]
    | NotFoundModelInConfig !String

instance Exception ProjectException

instance Show ProjectException where
    show (NotFoundAnyModel ms) =
        "Not found any model in config file: " ++ show ms
    show (NotFoundModelInConfig name) =
        "Not found model in config: " ++ show name



