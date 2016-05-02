{-# LANGUAGE RecordWildCards #-}

module Bimo.Types.Project where

import Data.List
import qualified Data.Map as M
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

import Bimo.Types.Config.Project

type Topology = [[String]]

data ModelEntity = ModelEntity
    { modelName :: !String
    , pipesToRead :: ![String]
    , pipesToWrite :: ![String]
    , execPath :: !FilePath
    , execArgs :: ![String]
    } deriving Show


modelsFromTopology :: Topology -> M.Map String ModelEntity
modelsFromTopology t =
    let pairs = topologyToPairs t
        ms    = uniqueModels t
     in foldl' addPipes ms pairs
  where
    addPipes ms (prod, cons, name) =
        M.update (\m@(ModelEntity _ _ w _ _) ->
            Just m{pipesToWrite = name : w}) prod $
        M.update (\m@(ModelEntity _ r _ _ _) ->
            Just m{pipesToRead = name : r}) cons
        ms
    uniqueModels t =
        let uniqueList = nub $ concat t
        in foldl' func M.empty uniqueList
      where
        func acc m =
            let model = ModelEntity m [] [] "" []
            in M.insert m model acc

fillModels :: (MonadThrow m, MonadIO m)
           => Project
           -> Path Abs Dir
           -> Path Rel Dir
           -> Path Rel Dir
           -> m (M.Map String ModelEntity)
fillModels (Project uModels lModels t) libRoot projectRoot execDir = do
    let tModels = modelsFromTopology t
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


topologyToPairs :: Topology -> [(String, String, String)]
topologyToPairs = concatMap toPairs
  where
    toPairs [] = []
    toPairs [x1] = []
    toPairs [x1, x2] = [(x1, x2, x1 ++ "-" ++ x2)]
    toPairs (x1:x2:xs) = (x1, x2, x1 ++ "-" ++ x2) : toPairs(x2 : xs)

topologyToPipes :: Topology -> [String]
topologyToPipes t = map (\(_, _, p) -> p) $ topologyToPairs t

data ProjectException
    = NotFoundAnyModel ![String]
    | NotFoundModelInConfig !String

instance Exception ProjectException

instance Show ProjectException where
    show (NotFoundAnyModel ms) =
        "Not found any model in config file: " ++ show ms
    show (NotFoundModelInConfig name) =
        "Not found model in config: " ++ show name



