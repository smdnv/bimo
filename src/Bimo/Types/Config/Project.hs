{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Project config

module Bimo.Types.Config.Project where

import Data.Yaml
import Data.Yaml.Pretty
import Data.Foldable
import qualified Data.ByteString as B
import Data.Aeson.Encode.Pretty (keyOrder)

data Project = Project
    { userModels :: !(Maybe [UserModel])
    , libModels  :: !(Maybe [LibModel])
    , topology   :: ![[String]]
    } deriving (Eq, Show)

data UserModel = UserModel
    { userModelName :: !String
    , userModelArgs :: ![String]
    } deriving (Eq, Show)

data LibModel = LibModel
    { libModelName :: !String
    , libModelCategory :: !String
    , libModelArgs :: ![String]
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON (Object v) =
        Project <$> v .:? "userModels"
                <*> v .:? "libModels"
                <*> v .: "topology"

instance ToJSON Project where
    toJSON Project{..} = object
      [ "userModels" .= userModels
      , "libModels"  .= libModels
      , "topology"   .= topology
      ]

instance FromJSON UserModel where
    parseJSON (Object v) =
        UserModel <$> v .: "name"
                  <*> v .: "execArgs"

instance FromJSON LibModel where
    parseJSON (Object v) =
        LibModel  <$> v .: "name"
                  <*> v .: "category"
                  <*> v .: "execArgs"

instance ToJSON UserModel where
    toJSON UserModel{..} = object
      [ "name"     .= userModelName
      , "execArgs" .= userModelArgs
      ]

instance ToJSON LibModel where
    toJSON LibModel{..} = object
      [ "name"     .= libModelName
      , "category" .= libModelCategory
      , "execArgs" .= libModelArgs
      ]

emptyProjectConfig :: B.ByteString
emptyProjectConfig =
    let o = keyOrder [ "userModels"
                     , "libModels"
                     , "topology"
                     ]
        c = setConfCompare o defConfig
        p = Project (Just []) (Just []) [[]]
     in encodePretty c p
