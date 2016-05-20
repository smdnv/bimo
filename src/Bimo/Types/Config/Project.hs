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
        , userModelStdIn :: !(Maybe FilePath)
        , userModelStdOut :: !(Maybe FilePath)
        , userModelStdErr :: !(Maybe FilePath)
        }
    | LibModel
        { libModelName :: !String
        , libModelCategory :: !String
        , libModelArgs :: ![String]
        , libModelStdIn :: !(Maybe FilePath)
        , libModelStdOut :: !(Maybe FilePath)
        , libModelStdErr :: !(Maybe FilePath)
        } deriving Show

instance Eq ModelConfig where
    (LibModel n1 c1 _ _ _ _) == (LibModel n2 c2 _ _ _ _) = n1 == n2 && c1 == c2
    (UserModel n1 _ _ _ _) == (UserModel n2 _ _ _ _) = n1 == n2
    _ == _ = False

instance FromJSON Project where
    parseJSON (Object v) =
        Project <$> v .:? "user-models"
                <*> v .:? "lib-models"
                <*> v .: "topology"

instance ToJSON Project where
    toJSON Project{..} = object
      [ "user-models" .= userModels
      , "lib-models"  .= libModels
      , "topology"    .= topology
      ]

instance FromJSON ModelConfig where
    parseJSON (Object v) = asum
        [ UserModel <$> v .: "user-model-name"
                    <*> v .: "exec-args"
                    <*> v .:? "stdin"
                    <*> v .:? "stdout"
                    <*> v .:? "stderr"
        , LibModel  <$> v .: "lib-model-name"
                    <*> v .: "category"
                    <*> v .: "exec-args"
                    <*> v .:? "stdin"
                    <*> v .:? "stdout"
                    <*> v .:? "stderr"
        ]

instance ToJSON ModelConfig where
    toJSON UserModel{..} = object
      [ "user-model-name" .= userModelName
      , "exec-args"       .= userModelArgs
      , "stdin"           .= userModelStdIn
      , "stdout"          .= userModelStdOut
      , "stderr"          .= userModelStdErr
      ]
    toJSON LibModel{..} = object
      [ "lib-model-name" .= libModelName
      , "category"       .= libModelCategory
      , "exec-args"      .= libModelArgs
      , "stdin"          .= libModelStdIn
      , "stdout"         .= libModelStdOut
      , "stderr"         .= libModelStdErr
      ]

emptyProjectConfig :: Project
emptyProjectConfig = Project Nothing Nothing [[]]

encodeProjectConfig :: Project -> B.ByteString
encodeProjectConfig p =
    let o = keyOrder [ "user-models"
                     , "lib-models"
                     , "topology"
                     , "user-model-name"
                     , "lib-model-name"
                     , "categor"
                     , "exec-args"
                     , "stdin"
                     , "stdout"
                     , "stderr"
                     ]
        c = setConfCompare o defConfig
     in encodePretty c p
