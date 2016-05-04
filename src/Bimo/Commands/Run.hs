{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Build model or project

module Bimo.Commands.Run
    ( run
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Process
import System.Exit
import System.Posix.Files

import Bimo.Types.Env
import Bimo.Types.Project
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Config
import Bimo.Project

run :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m)
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
        let procs = modelsToProcs (fromAbsDir tmpDir) models

        createPipes pipes
        handles <- runProcs procs
        exitCodes <- waitProcs handles
        mapM_ processExitCodes $ M.toList exitCodes
        )
  where
      createPipes pipes = liftIO $ mapM_ (`createNamedPipe` namedPipeMode) pipes
      processExitCodes (k, ec)  = unless (ec == ExitSuccess) (error "model fails")

modelsToProcs :: FilePath
              -> M.Map String ModelEntity
              -> M.Map String CreateProcess
modelsToProcs dir = M.map func
  where
    func ModelEntity{..} =
        let p = proc execPath $ [ intercalate ":" pipesToRead
                                , intercalate ":" pipesToWrite
                                ] ++ execArgs
         in p { delegate_ctlc = True
              , cwd = Just dir
              }

runProcs :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m)
         => M.Map String CreateProcess
         -> m (M.Map String ProcessHandle)
runProcs = M.foldlWithKey func (return M.empty)
  where
    func acc k p = do
        acc' <- acc
        (_, _, _, ph) <- liftIO $ createProcess p
        logInfoN $ "Run model: "
        return $ M.insert k ph acc'

waitProcs :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m)
         => M.Map String ProcessHandle
         -> m (M.Map String ExitCode)
waitProcs = M.foldlWithKey func (return M.empty)
  where
    func acc k ph = do
        acc' <- acc
        ec <- liftIO $ waitForProcess ph
        return $ M.insert k ec acc'
