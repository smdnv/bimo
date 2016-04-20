-- | Module to interact with any app config

module Bimo.Config where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Yaml
import Path
-- import Path.IO

-- import Bimo.Types.Env
-- import Bimo.Types.Config.Project
import Bimo.Types.Config.Model


obtainModelConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                  => Path Rel File
                  -> m Model
obtainModelConfig p = do
    let file = fromRelFile p
    decoded <- liftIO $ decodeFileEither file
    case decoded of
        Left e -> throwM e
        Right m -> return m


