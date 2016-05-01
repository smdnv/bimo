{-# LANGUAGE RecordWildCards #-}

module Bimo.Types.Project where

import Data.List
import qualified Data.Map as M
import Control.Monad.Catch
-- import Data.Foldable


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

fillModels :: MonadThrow m
           => Topology
           -> Maybe [ModelConfig]
           -> Maybe [ModelConfig]
           -> m [ModelEntity]
fillModels t uModels lModels = do
    let tModels = modelsFromTopology t
    pModels <- case (uModels, lModels) of
        (Nothing, Nothing) -> throwM $
            NotFoundAnyModel $ M.foldlWithKey (\ks k _ -> k:ks) [] tModels
        (Just uModels', Nothing) -> return $ toMap' uModels'
        (Nothing, Just lModels') -> return $ toMap' lModels'
        (Just uModels', Just lModels') -> return $ toMap' (uModels' ++ lModels')
    fillModels' pModels tModels
    --     (Just uModels', Nothing) -> fillModels' (toMap' uModels') tModels
    --     (Nothing, Just lModels') -> fillModels' (toMap' lModels') tModels
    --     (Just uModels', Just lModels') ->
    --         fillModels' (toMap' (uModels' ++ lModels')) tModels
  where
    toMap' = foldl' func M.empty
      where
        func acc m@(UserModel n _) = M.insert n m acc
        func acc m@(LibModel n _ _) = M.insert n m acc
    fillModels' :: M.Map String ModelConfig
                -> M.Map String ModelEntity
                -> m [ModelEntity]
    fillModels' pModels tModels = do
        -- mapM find tModels
      where
        find pModels ModelEntity{..} = do
            case M.lookup modelName models of
                Nothing -> throwM $ NotFoundModelInConfig modelName
                Just (UserModel _ execArgs) -> return ModelEntity{..}
                Just (LibModel _ _ execArgs) -> return ModelEntity{..}




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



