{-# LANGUAGE OverloadedStrings #-}

-- | Model config
-- example
-- @
-- name: hamming_coder
-- category: coders
-- version: 0.0.8
-- descr: |
--   model descr
--   usage ...
--   args ...
--   etc
-- lang: c
-- libs:
--   - pipes
--   - hamming
--   @

module Bimo.Types.Config.Model where

import Data.Yaml

data Model = Model
    { modelName :: !String
    , category :: !String
    , version :: !String
    , descr :: !String
    , lang :: !String
    , libs :: ![String]
    } deriving (Eq, Show)

instance FromJSON Model where
  parseJSON (Y.Object v) = do
    modelName <- v .: "name"
    category <- v .: "category"
    version <- v .: "version"
    descr <- v .: "descr"
    lang <- v .: "lang"
    libs <- v .: "libs"
    return Model{..}
