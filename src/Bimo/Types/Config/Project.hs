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
    { models   :: ![ModelConfig]
    , topology :: ![[String]]
    } deriving (Eq, Show)

data ModelConfig
  = UserModel
    { srcFile   :: !String
    , modelLang :: !String
    , execArgs  :: ![String]
    }
  | LibModel
    { modelName :: !String
    , version   :: !String
    , execArgs  :: ![String]
    }
  deriving (Eq, Show)

instance FromJSON Project where
    parseJSON (Object v) =
      Project <$> v .: "models" <*> v .: "topology"

instance ToJSON Project where
    toJSON Project{..} = object
      [ "models"   .= models
      , "topology" .= topology
      ]

instance FromJSON ModelConfig where
    parseJSON (Object v) = asum
      [ UserModel <$> v .: "srcFile" <*> v .: "modelLang" <*> v .: "execArgs"
      , LibModel <$> v .: "modelName" <*> v .: "version" <*> v .: "execArgs"
      ]

instance ToJSON ModelConfig where
    toJSON UserModel{..} = object
      [ "srcFile"   .= srcFile
      , "modelLang" .= modelLang
      , "execArgs"  .= execArgs
      ]
    toJSON LibModel{..} = object
      [ "modelName" .= modelName
      , "version"   .= version
      , "execArgs"  .= execArgs
      ]

emptyProjectConfig :: B.ByteString
emptyProjectConfig =
    let o = keyOrder [ "models"
                     , "topology"
                     ]
        c = setConfCompare o defConfig
        p = Project [] [[]]
     in encodePretty c p
