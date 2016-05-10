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
    { modelName       :: !String
    , category        :: !String
    , descr           :: !String
    , language        :: !String
    , srcFiles        :: !(Maybe [String])
    , libs            :: !(Maybe [String])
    , userBuildScript :: !(Maybe String)
    } deriving (Eq, Show)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelName       <- v .: "name"
    category        <- v .: "category"
    descr           <- v .: "descr"
    language        <- v .: "language"
    srcFiles        <- v .:? "source"
    libs            <- v .:? "libs"
    userBuildScript <- v .:? "user-build-script"
    return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object
    [ "name"              .= modelName
    , "category"          .= category
    , "descr"             .= descr
    , "language"          .= language
    , "source"            .= srcFiles
    , "libs"              .= libs
    , "user-build-script" .= userBuildScript
    ]

emptyModelConfig :: String
                 -> Maybe String
                 -> Maybe String
                 -> B.ByteString
emptyModelConfig name category language =
    let o = keyOrder [ "name"
                     , "category"
                     , "descr"
                     , "language"
                     , "source"
                     , "libs"
                     , "user-build-script"
                     ]
        cat   = fromMaybe "none" category
        lang  = fromMaybe "" language
        descr = "model description there"
        conf  = setConfCompare o defConfig
        model = Model name cat descr lang Nothing Nothing Nothing
     in encodePretty conf model
