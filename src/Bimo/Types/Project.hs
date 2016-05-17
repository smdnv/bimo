module Bimo.Types.Project where

import Data.List
import qualified Data.Map as M

import Bimo.Types.Config.Project

type Topology = [[String]]

data ModelEntity = ModelEntity
    { modelName :: !String
    , pipesToRead :: ![String]
    , pipesToWrite :: ![String]
    , execPath :: !FilePath
    , execArgs :: ![String]
    , modelStdIn :: !(Maybe FilePath)
    , modelStdOut :: !(Maybe FilePath)
    , modelStdErr :: !(Maybe FilePath)
    } deriving Show


modelsFromTopology :: Topology -> M.Map String ModelEntity
modelsFromTopology t =
    let pairs = topologyToPairs t
        ms    = uniqueModels t
     in foldl' addPipes ms pairs
  where
    addPipes ms (prod, cons, name) =
        M.update (\m@(ModelEntity _ _ w _ _ _ _ _) ->
            Just m{pipesToWrite = name : w}) prod $
        M.update (\m@(ModelEntity _ r _ _ _ _ _ _) ->
            Just m{pipesToRead = name : r}) cons
        ms
    uniqueModels t =
        let uniqueList = nub $ concat t
        in foldl' func M.empty uniqueList
      where
        func acc m =
            let model = ModelEntity m [] [] "" [] Nothing Nothing Nothing
            in M.insert m model acc


topologyToPairs :: Topology -> [(String, String, String)]
topologyToPairs = concatMap toPairs
  where
    toPairs [] = []
    toPairs [x1] = []
    toPairs [x1, x2] = [(x1, x2, x1 ++ "-" ++ x2)]
    toPairs (x1:x2:xs) = (x1, x2, x1 ++ "-" ++ x2) : toPairs(x2 : xs)

topologyToPipes :: Topology -> [String]
topologyToPipes t = map (\(_, _, p) -> p) $ topologyToPairs t


toLibModel :: String -> ModelConfig -> ModelConfig
toLibModel category (UserModel name args stdin' stdout' stderr') =
    LibModel name category args stdin' stdout' stderr'

toUserModel :: ModelConfig -> ModelConfig
toUserModel (LibModel name _ args stdin' stdout' stderr') =
    UserModel name args stdin' stdout' stderr'
