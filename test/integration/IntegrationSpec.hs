{-# LANGUAGE RecordWildCards #-}

import System.Directory
import System.IO.Temp
import System.Process
import System.Environment
import System.Exit
import System.FilePath
import Control.Exception
import qualified Data.Map as M
import Test.Hspec

data TestEnv = TestEnv
    { currDir :: String
    , runghc :: String
    , app :: String
    , env' :: [(String, String)]
    }

main :: IO ()
main =
    withSystemTempDirectory "home" $ \newHome -> do
      currDir <- canonicalizePath "test/integration"
      runghc <- findExec "runghc"
      app <- findExec "bimo"
      env <- getEnvironment
      let env' = M.toList
               $ M.insert "APP" app
               $ M.insert "HOME" newHome
               $ M.fromList env
          testEnv = TestEnv{..}

      hspec $
        context "New command tests" $
          test "create-empty-project" testEnv

  where
    findExec s = do
      p <- findExecutable s
      case p of
          Just path -> return path
          Nothing -> error $ "not found: " ++ s


test :: String -> TestEnv -> Spec
test name TestEnv{..} =
    it name $
      withSystemTempDirectory name $ \newDir -> do
        let main = currDir </> "tests" </> name </> "Main.hs"
            lib  = currDir </> "lib"
            p = (proc runghc [ "-i" ++ lib
                             , main]) { cwd = Just newDir, env = Just env' }
        (ec, out, err) <- readCreateProcessWithExitCode p ""
        if ec /= ExitSuccess
            then throwIO $ TestFailure ec out err
            else return ()

data TestFailure = TestFailure ExitCode String String

instance Show TestFailure where
    show (TestFailure ec out err) = concat
        [ "\n"
        , "\nexit code: \n" ++ show ec ++ "\n"
        , "\nstdout:    \n" ++ out
        , "\nstderr:    \n" ++ err
        ]

instance Exception TestFailure


