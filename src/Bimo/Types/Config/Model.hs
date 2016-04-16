{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import Data.Yaml.Pretty
import qualified Data.ByteString as B
import Data.Aeson.Encode.Pretty (keyOrder)

data Model = Model
    { modelName :: !String
    , category  :: !String
    , version   :: !String
    , descr     :: !String
    , lang      :: !String
    , libs      :: ![String]
    } deriving (Eq, Show)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelName <- v .: "name"
    category  <- v .: "category"
    version   <- v .: "version"
    descr     <- v .: "descr"
    lang      <- v .: "lang"
    libs      <- v .: "libs"
    return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object
    [ "name"     .= modelName
    , "category" .= category
    , "version"  .= version
    , "descr"    .= descr
    , "lang"     .= lang
    , "libs"     .= libs
    ]

-- add opts: lang, category
emptyModelConfig :: String -> B.ByteString
emptyModelConfig name =
    let o = keyOrder [ "name"
                     , "category"
                     , "version"
                     , "descr"
                     , "lang"
                     , "libs"
                     ]
        c = setConfCompare o defConfig
        m = Model name "none" "0.0.1" "" "" []
     in encodePretty c m
