module TestLib where

import System.Environment
import System.Directory
import System.IO
import System.Process
import System.Exit
import Control.Monad
import Data.List

run :: FilePath -> [String] -> IO (ExitCode, String, String)
run cmd args = do
    logInfo $ "Run: " ++ cmd ++ " " ++ unwords args
    readCreateProcessWithExitCode (proc cmd args) ""

bimo :: [String] -> IO ()
bimo args = do
    exec <- getEnv "BIMO"
    (ec, out, err) <- run exec args
    -- putStrLn out
    -- putStrLn err
    unless (ec == ExitSuccess) (error "Should successfully finish, but fail")

bimoFail :: [String] -> IO String
bimoFail args = do
    exec <- getEnv "BIMO"
    (ec, _, err) <- run exec args
    unless (ec /= ExitSuccess) (error "Should fail, but successfully finish")
    return err

bimoFailAndStderrContent :: [String] -> [String] -> IO ()
bimoFailAndStderrContent args ms = do
    err <- bimoFail args
    stringContents err ms

doesExist :: FilePath -> IO ()
doesExist p = do
    logInfo $ "Does exist: " ++ p
    exists <- doesFileExist p
    if exists
       then return ()
       else do exists <- doesDirectoryExist p
               unless exists . error $ "Does not exist: " ++ p

fileContents :: FilePath -> [String] -> IO ()
fileContents file ms = do
    content <- readFile file
    stringContents content ms

stringContents :: String -> [String] -> IO ()
stringContents content ms = do
    mapM_ (testMatch content) ms
  where
    testMatch content match =
        unless (isInfixOf match content) $
            error $ "Not find in file: " ++ match ++ "\n"
                                         ++ content

logInfo :: String -> IO ()
logInfo = putStrLn

