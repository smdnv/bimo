module TestLib where

import System.Environment
import System.Directory
import System.IO
import System.Process
import System.Exit
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

run :: FilePath -> [String] -> IO ExitCode
run cmd args = do
    logInfo $ "Run: " ++ cmd ++ " " ++ unwords args
    (_, _, _, ph) <- createProcess (proc cmd args)
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

doesExist :: FilePath -> IO ()
doesExist p = do
    logInfo $ "Does exist: " ++ p
    exists <- doesFileExist p
    if exists
       then return ()
       else do exists <- doesDirectoryExist p
               unless exists . error $ "Does not exist: " ++ p

fileContents :: FilePath -> [String] -> IO ()
fileContents file match = do
    let match' = map T.pack match
    content <- T.readFile file
    mapM_ (testMatch content) match'
  where
    testMatch content match =
        unless (T.isInfixOf match content) $
            error $ "Not find in file: " ++ T.unpack match ++ "\n"
                                         ++ T.unpack content


logInfo :: String -> IO ()
logInfo = putStrLn

