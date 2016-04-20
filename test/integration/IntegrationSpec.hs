{-# LANGUAGE RecordWildCards #-}

import Data.Foldable
import System.Directory
import System.IO.Temp
import System.Process
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Files
import Control.Exception
import qualified Data.Map as M
import Test.Hspec

data TestEnv = TestEnv
    { currDir :: !FilePath
    , runghc  :: !FilePath
    , app     :: !FilePath
    , home    :: !FilePath
    , appData :: !FilePath
    , env'    :: ![(String, String)]
    }

main :: IO ()
main =
    withSystemTempDirectory "home" $ \newHome -> do
        currDir <- canonicalizePath "test/integration"
        runghc  <- findExec "runghc"
        app     <- findExec "bimo"
        env     <- getEnvironment
        let home = newHome
            appData = home </> ".bimo"
            env' = M.toList
                $ M.insert "BIMO" app
                $ M.insert "BIMO_DATA" appData
                $ M.insert "HOME" newHome
                $ M.fromList env
            testEnv = TestEnv{..}

        hspec $ do
            context "New command tests" $ do
                test "create-empty-project" testEnv
                test "create-empty-model" testEnv
            context "Build command tests" $ do
                test "build-simple-model" testEnv

  where
    findExec s = do
        p <- findExecutable s
        case p of
            Just path -> return path
            Nothing   -> error $ "not found: " ++ s


test :: String -> TestEnv -> Spec
test name TestEnv{..} =
    it name $
        withSystemTempDirectory name $ \newDir -> do
            -- removeDirectoryRecursive appData
            let testDir = currDir </> "tests" </> name
                main = testDir </>  "Main.hs"
                lib  = currDir </> "lib"
                p    = (proc runghc [ "-i" ++ lib
                                    , main]) { cwd = Just newDir
                                            , env = Just env' }

            copyFiles (testDir </> "files") newDir

            (ec, out, err) <- readCreateProcessWithExitCode p ""
            if ec /= ExitSuccess
                then throwIO $ TestFailure ec out err
                else return ()

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcRoot dstRoot = do
    (srcDirs, srcFiles) <- getDirContentsRecur srcRoot
    let stopDir     = addTrailingPathSeparator $ last $ splitPath srcRoot
        convertPath = map ( (</>) dstRoot
                          . joinPath
                          . tail
                          . dropWhile (/= stopDir)
                          . splitPath
                          )
        (dstDirs, dstFiles) = (convertPath srcDirs, convertPath srcFiles)
        dirs  = zip srcDirs dstDirs
        files = zip srcFiles dstFiles

    mapM_ (uncurry createSymbolicLink) dirs
    mapM_ (uncurry createSymbolicLink) files

getDirContents :: FilePath -> IO ([FilePath], [FilePath])
getDirContents path = do
    cs <- getDirectoryContents path
    let content = filter (`notElem` [".", ".."]) cs
    foldlM func ([], []) content
  where
    func (dirs, files) p = do
        let relPath = path </> p
        testDir <- doesDirectoryExist relPath
        return $
            if testDir
            then (relPath : dirs, files)
            else (dirs, relPath : files)

getDirContentsRecur :: FilePath -> IO ([FilePath], [FilePath])
getDirContentsRecur path = do
    contents <- getDirContents path
    foldl' mappend contents <$> mapM getDirContentsRecur (fst contents)

data TestFailure = TestFailure ExitCode String String

instance Show TestFailure where
    show (TestFailure ec out err) = concat
        [ "\n"
        , "\nexit code: \n" ++ show ec ++ "\n"
        , "\nstdout:    \n" ++ out
        , "\nstderr:    \n" ++ err
        ]

instance Exception TestFailure


