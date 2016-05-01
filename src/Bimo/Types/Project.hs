module Bimo.Types.Project where

import Data.List
import qualified Data.Map as M

topology = [ ["model1", "model2", "model3", "model4", "model5"]
           , ["model1", "model3", "model5" ]
           , ["model5", "model1"]
           ]

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

topologyToPairs :: Topology -> [(String, String, String)]
topologyToPairs = concatMap toPairs
  where
    toPairs [] = []
    toPairs [x1] = []
    toPairs [x1, x2] = [(x1, x2, x1 ++ "-" ++ x2)]
    toPairs (x1:x2:xs) = (x1, x2, x1 ++ "-" ++ x2) : toPairs(x2 : xs)

topologyToPipes :: Topology -> [String]
topologyToPipes t = map (\(_, _, p) -> p) $ topologyToPairs t

