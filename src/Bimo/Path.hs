-- | Path helpers
module Bimo.Path where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO

whenDirExists :: MonadIO m
              => Path b Dir
              -> m ()
              -> m ()
whenDirExists path action = do
    exists <- doesDirExist path
    when exists action

whenFileExists :: MonadIO m
               => Path b File
               -> m ()
               -> m ()
whenFileExists path action = do
    exists <- doesFileExist path
    when exists action

unlessDirExists :: MonadIO m
                => Path b Dir
                -> m ()
                -> m ()
unlessDirExists path action = do
    exists <- doesDirExist path
    unless exists action

unlessFileExists :: MonadIO m
                 => Path b File
                 -> m ()
                 -> m ()
unlessFileExists path action = do
    exists <- doesFileExist path
    unless exists action
