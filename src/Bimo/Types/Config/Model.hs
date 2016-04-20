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
-- language: c
-- libs:
--   - pipes
--   - hamming
--   @

module Bimo.Types.Config.Model where

import Data.Yaml
import Data.Yaml.Pretty
import qualified Data.ByteString as B
import Data.Aeson.Encode.Pretty (keyOrder)
import Data.Maybe (fromMaybe)

data Model = Model
    { modelName :: !String
    , category  :: !String
    , version   :: !String
    , descr     :: !String
    , language  :: !String
    , libs      :: ![String]
    } deriving (Eq, Show)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelName <- v .: "name"
    category  <- v .: "category"
    version   <- v .: "version"
    descr     <- v .: "descr"
    language  <- v .: "language"
    libs      <- v .: "libs"
    return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object
    [ "name"     .= modelName
    , "category" .= category
    , "version"  .= version
    , "descr"    .= descr
    , "language" .= language
    , "libs"     .= libs
    ]

-- add opts: lang, category
emptyModelConfig :: String
                 -> Maybe String
                 -> Maybe String
                 -> B.ByteString
emptyModelConfig name category language =
    let o = keyOrder [ "name"
                     , "category"
                     , "version"
                     , "descr"
                     , "language"
                     , "libs"
                     ]
        cat   = fromMaybe "none" category
        lang  = fromMaybe "" language
        descr = "model description there"
        conf  = setConfCompare o defConfig
        model = Model name cat "0.0.1" descr lang []
     in encodePretty conf model
