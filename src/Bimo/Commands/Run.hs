{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Build model or project

module Bimo.Commands.Run
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Monoid
import Data.Bits ((.|.))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted
import Path
import Path.IO
import System.Process
import System.Exit
import System.Posix.Files

import Bimo.Types.Env
import Bimo.Types.Project
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Model
import Bimo.Config
import Bimo.Project

run :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m, MonadBaseControl IO m)
    => m ()
run = do
    pConf  <- asks projectConfig
    rDir   <- asks runDir
    curDir <- getCurrentDir
    p      <- readProjectConfig $ curDir </> pConf
    models <- fillModels p

    withSystemTempDir rDir (\tmpDir -> do
        pipes <- mapM (\p -> do
            pipe <- parseRelFile p
            return $ fromAbsFile $ tmpDir </> pipe ) $ topologyToPipes $ topology p
        let procs = modelsToProcs tmpDir models

        bracketOnError (createPipes pipes >> runProcs procs)
                       terminate
                       (mapConcurrently waitProc >=> \_ -> return ())
        )
  where
      createPipes pipes = liftIO $
          mapM_ (`createNamedPipe` (namedPipeMode .|. 0o666)) pipes
      terminate ps = do
        logWarnN "Terminate models"
        mapM_ (\(n, ph) -> do liftIO $ terminateProcess ph
                              logWarnN $ "Kill model: " <> n) ps


modelsToProcs :: Path Abs Dir
              -> M.Map String ModelEntity
              -> M.Map String CreateProcess
modelsToProcs dir = M.map func
  where
    func ModelEntity{..} =
        let p = proc execPath $ [ intercalate ":" pipesToRead
                                , intercalate ":" pipesToWrite
                                ] ++ execArgs
         in p { delegate_ctlc = True
              , cwd = Just $ fromAbsDir dir
              }

runProcs :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m)
         => M.Map String CreateProcess
         -> m [(T.Text, ProcessHandle)]
runProcs = mapM func . M.toList
  where
    func (n, p) = do
        let name = T.pack n
        (_, _, _, ph) <- liftIO $ createProcess p
        logInfoN $ "Start model: " <> name
        return (name,  ph)

waitProc :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadBaseControl IO m)
         => (T.Text, ProcessHandle)
         -> m ()
waitProc (n, ph) = do
    ec <- liftIO $ waitForProcess ph
    unless (ec == ExitSuccess) $ throwM $ ModelFailure n
    logInfoN $ "Finish model: " <> n

data RunException
    = ModelFailure T.Text

instance Exception RunException

instance Show RunException where
    show (ModelFailure name) =
        "Model failure: " ++ show name
