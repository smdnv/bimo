{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Build model or project

module Bimo.Commands.Run
    ( run
    , RunOpts(..)
    ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Maybe
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
import System.IO
import System.Process
import System.Exit
import System.Posix.Files
import System.Clock

import Bimo.Types.Env
import Bimo.Types.Project
import Bimo.Types.Config.Project
import Bimo.Types.Config.Model

import Bimo.Model
import Bimo.Config
import Bimo.Project

data RunOpts = NotSilent | Silent
    deriving Show

run :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m, MonadReader Env m, MonadBaseControl IO m)
    => RunOpts
    -> m ()
run opts = do
    pConf  <- asks projectConfig
    rDir   <- asks runDir
    curDir <- getCurrentDir
    p      <- readProjectConfig $ curDir </> pConf
    models <- fillModels p

    withSystemTempDir rDir (\tmpDir -> do
        pipes <- mapM (\p -> do
            pipe <- parseRelFile p
            return $ fromAbsFile $ tmpDir </> pipe ) $ topologyToPipes $ topology p

        let models' = M.toList models

        start <- liftIO $ getTime Monotonic

        bracketOnError (do createPipes pipes
                           mapM (modelToProc opts tmpDir >=> runModel) models')
                       terminate
                       (mapConcurrently waitProc >=> \_ -> return ())

        end <- liftIO $ getTime Monotonic

        let diff = diffTimeSpec end start
        logInfoN $ T.concat [ "Elapsed time: "
                            , T.pack $ show $ sec diff
                            , "."
                            , T.pack $ take 3 $ show $ nsec diff
                            , " sec."
                            ]
        )
  where
      createPipes pipes = liftIO $
          mapM_ (`createNamedPipe` (namedPipeMode .|. 0o666)) pipes
      terminate ps = do
        logWarnN "Terminate models"
        mapM_ (\(n, ph) -> do liftIO $ terminateProcess ph
                              logWarnN $ "Kill model: " <> n) ps

modelToProc :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m)
            => RunOpts
            -> Path Abs Dir
            -> (String, ModelEntity)
            -> m (T.Text, CreateProcess)
modelToProc flag dir (name, ModelEntity{..}) = do
    let args = [ intercalate ":" pipesToRead
               , intercalate ":" pipesToWrite
               ] ++ execArgs
        n = T.pack name
        p = (proc execPath args) { delegate_ctlc = True
                                 , cwd = Just $ fromAbsDir dir
                                 }

    procStdIn <- case (modelStdIn, flag) of
                     (Just inFile, _) -> fileToProcStream inFile ReadMode
                     (Nothing, _) -> return Inherit

    procStdOut <- case (modelStdOut, flag) of
                      (Just outFile, _) -> fileToProcStream outFile WriteMode
                      (Nothing, Silent) -> fileToProcStream "/dev/null" WriteMode
                      (Nothing, NotSilent) -> return Inherit

    procStdErr <- case (modelStdErr, flag) of
                      (Just errFile, _) -> fileToProcStream errFile WriteMode
                      (Nothing, Silent) -> fileToProcStream "/dev/null" WriteMode
                      (Nothing, NotSilent) -> return Inherit

    logInfoN $ T.concat [ "Start model: " <> n
                        , "\nRun args: "
                        , T.pack $ show args
                        , "\nStdin:  "
                        , T.pack $ maybe "inherited" show modelStdIn
                        , "\nStdout: "
                        , T.pack $ maybe "inherited" show modelStdOut
                        , "\nStderr: "
                        , T.pack $ maybe "inherited" show modelStdErr
                        ]

    return
        (n, p { std_in = procStdIn, std_out = procStdOut, std_err = procStdErr })
  where
    fileToProcStream name mode = do
        hd <- liftIO $ openFile name mode
        return $ UseHandle hd

runModel :: (MonadIO m, MonadThrow m, MonadMask m, MonadLogger m)
         => (T.Text, CreateProcess)
         -> m (T.Text, ProcessHandle)
runModel (n, p) = do
    (_, _, _, ph) <- liftIO $ createProcess p
    return (n,  ph)

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
