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

userConfirm :: (MonadIO m, MonadThrow m) => m () -> m () -> m ()
userConfirm log yes = do
    log
    line <- liftIO getLine
    case line of
        "yes"   -> yes
        "no"    -> liftIO $ putStrLn "Cancel command"
        invalid -> throwM $ AbortCommand invalid

data RenameException
    = AbortCommand !String

instance Exception RenameException

instance Show RenameException where
    show (AbortCommand input) =
        "Abort command, invalid input: " ++ show input
