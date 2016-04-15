module TestLib where

import System.Environment
import System.Directory
import System.IO
import System.Process
import System.Exit
import Control.Monad

run :: FilePath -> [String] -> IO ExitCode
run cmd args = do
    logInfo $ "Run: " ++ cmd ++ " " ++ unwords args
    (_, _, _, ph) <- createProcess (proc cmd args)
    waitForProcess ph

bimo :: [String] -> IO ()
bimo args = do
    exec <- getEnv "APP"
    ec <- run exec args
    unless (ec == ExitSuccess) (error "fail")

doesExist :: FilePath -> IO ()
doesExist p = do
    exists <- doesFileExist p
    if exists
       then return ()
       else do exists <- doesDirectoryExist p
               unless exists $ error "Directory or file does not exist"

logInfo :: String -> IO ()
logInfo = putStrLn

