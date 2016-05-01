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
    { userModels :: !(Maybe [ModelConfig])
    , libModels  :: !(Maybe [ModelConfig])
    , topology   :: ![[String]]
    } deriving (Eq, Show)

data ModelConfig
    = UserModel
        { userModelName :: !String
        , userModelArgs :: ![String]
        }
    | LibModel
        { libModelName :: !String
        , libModelCategory :: !String
        , libModelArgs :: ![String]
        } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON (Object v) =
        Project <$> v .:? "user-models"
                <*> v .:? "lib-models"
                <*> v .: "topology"

instance ToJSON Project where
    toJSON Project{..} = object
      [ "user-models" .= userModels
      , "lib-models"  .= libModels
      , "topology"   .= topology
      ]

instance FromJSON ModelConfig where
    parseJSON (Object v) = asum
        [ UserModel <$> v .: "user-model-name"
                    <*> v .: "exec-args"
        , LibModel  <$> v .: "lib-model-name"
                    <*> v .: "category"
                    <*> v .: "exec-args"
        ]

instance ToJSON ModelConfig where
    toJSON UserModel{..} = object
      [ "user-model-name" .= userModelName
      , "exec-args"       .= userModelArgs
      ]
    toJSON LibModel{..} = object
      [ "lib-model-name" .= libModelName
      , "category"       .= libModelCategory
      , "exec-args"      .= libModelArgs
      ]

emptyProjectConfig :: B.ByteString
emptyProjectConfig =
    let o = keyOrder [ "user-models"
                     , "lib-models"
                     , "topology"
                     ]
        c = setConfCompare o defConfig
        p = Project (Just []) (Just []) [[]]
     in encodePretty c p
