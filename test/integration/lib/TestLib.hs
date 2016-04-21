module TestLib where

import System.Environment
import System.Directory
import System.IO
import System.Process
import System.Exit
import Control.Monad
import Data.List

run :: FilePath -> [String] -> IO ExitCode
run exec args = do
    logInfo $ "Run: " ++ exec ++ " " ++ unwords args
    (_, _, _, ph) <- createProcess (proc exec args)
    waitForProcess ph

bimo :: [String] -> IO ()
bimo args = do
    exec <- getEnv "BIMO"
    ec <- run exec args
    unless (ec == ExitSuccess) (error "Should successfully finish, but fail")

bimoFail :: [String] -> IO ()
bimoFail args = do
    exec <- getEnv "BIMO"
    ec <- run exec args
    unless (ec /= ExitSuccess) (error "Should fail, but successfully finish")

bimoFailAndStderrContent :: [String] -> [String] -> IO ()
bimoFailAndStderrContent args ms = do
    exec <- getEnv "BIMO"
    (ec, out, err) <- readCreateProcessWithExitCode (proc exec args) ""
    unless (ec /= ExitSuccess) (error "Should fail, but successfully finish")
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
stringContents content ms =
    mapM_ (testMatch content) ms
  where
    testMatch content match =
        unless (match `isInfixOf` content) $
            error $ "Not find in file: " ++ match ++ "\n"
                                         ++ content

logInfo :: String -> IO ()
logInfo = putStrLn

