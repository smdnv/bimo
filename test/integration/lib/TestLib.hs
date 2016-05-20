module TestLib where

import System.Environment
import System.Directory
import System.IO
import System.Process
import System.Exit
import Control.Monad
import Data.List

shouldFinish :: ExitCode -> IO ()
shouldFinish ec = unless (ec == ExitSuccess)
                         (error "Should successfully finish, but fail")

shouldFail :: ExitCode -> IO ()
shouldFail ec = when (ec == ExitSuccess)
                     (error "Should fail, but successfully finish")

run :: [String] -> String -> IO (ExitCode, String, String)
run args input = do
    exec <- getEnv "BIMO"
    logInfo $ "Run bimo with args: " ++ unwords args
    let p = proc exec args

    (ec, out, err) <- readCreateProcessWithExitCode p input
    unless (null out) (logInfo $ "--- bimo stdout:\n" ++ out)
    unless (null input) (logInfo $ "--- bimo stdin:\n" ++ input)
    unless (null err) (logInfo $ "--- bimo stderr:\n" ++ err)
    return (ec , out, err)

bimo :: [String] -> IO ()
bimo args = do
    (ec, _, _) <- run args ""
    shouldFinish ec

bimo' :: [String] -> String -> IO ()
bimo' args input = do
    (ec, _, _) <- run args input
    shouldFinish ec

bimoStdoutContent :: [String] -> [String] -> IO ()
bimoStdoutContent args ms = do
    (ec, out, _) <- run args ""
    shouldFinish ec
    stringContents out ms

bimoFail :: [String] -> IO ()
bimoFail args = do
    (ec, _, _) <- run args ""
    shouldFail ec

bimoFailStderrContent :: [String] -> [String] -> IO ()
bimoFailStderrContent args ms = do
    (ec, _, err) <- run args ""
    shouldFail ec
    stringContents err ms

bimoFailStderrContent' :: [String] -> [String] -> String -> IO ()
bimoFailStderrContent' args ms input = do
    (ec, _, err) <- run args input
    shouldFail ec
    stringContents err ms

existTest :: FilePath -> IO Bool
existTest p = do
    testFile <- doesFileExist p
    testDir <- doesDirectoryExist p
    case (testFile, testDir) of
        (True, False) -> return True
        (False, True) -> return True
        (_ ,_) -> return False

doesExist :: FilePath -> IO ()
doesExist p = do
    logInfo $ "Does exist: " ++ p
    exists <- existTest p
    unless exists $ error $ "Does not exist: " ++ p

doesNotExist :: FilePath -> IO ()
doesNotExist p = do
    logInfo $ "Does not exist: " ++ p
    exists <- existTest p
    when exists $ error $ "Does exist: " ++ p

fileContents :: FilePath -> [String] -> IO ()
fileContents file ms = do
    content <- readFile file
    stringContents content ms

stringContents :: String -> [String] -> IO ()
stringContents content = mapM_ (testMatch content)
  where
    testMatch content match =
        unless (match `isInfixOf` content) $
            error $ "Not find: " ++ match ++ "\n"
                                 ++ content

logInfo :: String -> IO ()
logInfo = putStrLn

