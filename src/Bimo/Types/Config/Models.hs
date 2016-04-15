{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Models library config
-- example
-- @
-- - category: coders
--   models:
--   - name: hamming
--     versions:
--     - 123f
--     - 456d
--   - name: rc
--     versions:
--     - 5sdf
-- - category: channels
--   models:
--   - name: bsc
--     versions: [df123]
-- - category: channels123
--   models: []
-- @

module Bimo.Types.Config.Models where

import Data.Yaml

type Models = [Category]

data Category = Category
    { category :: !String
    , models   :: ![Model]
    } deriving (Show, Eq)

data Model = Model
    { modelName :: !String
    , versions  :: ![String]
    } deriving (Show, Eq)

instance FromJSON Model where
  parseJSON (Y.Object v) = do
    modelName <- v .: "name"
    versions  <- v .: "versions"
    return Model{..}

instance FromJSON Category where
  parseJSON (Y.Object v) =
    Category <$> v .: "category" <*> v .: "models"

