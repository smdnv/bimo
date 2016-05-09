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
import Control.Monad
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
                 $ M.fromList env
            testEnv = TestEnv{..}

        hspec $ do
            describe "New command tests" $ do
                test "new-empty-project" testEnv
                test "new-empty-model" testEnv
                test "new-project-from-template" testEnv
                test "new-project-from-template-unpack" testEnv

            describe "Build command tests" $ do
                context "build model" $ do
                    test "build-simple-c-model" testEnv
                    test "build-simple-c-model-with-lib" testEnv
                    test "build-c-with-mult-files-and-libs" testEnv
                context "build project" $ do
                    test "build-project" testEnv

            describe "Run command tests" $ do
                test "run-project" testEnv

            describe "Add command tests" $ do
                test "add-model" testEnv
                test "add-template" testEnv

            describe "Delete command tests" $ do
                test "delete-template" testEnv
                test "delete-model" testEnv

            describe "Rename command tests" $ do
                test "rename-template" testEnv
                -- test "delete-model" testEnv

            describe "Unpack command tests" $ do
                test "unpack-model" testEnv

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
            removeDirRecur appData
            let testDir = currDir </> "tests" </> name
                main = testDir </>  "Main.hs"
                lib  = currDir </> "lib"
                p    = (proc runghc [ "-i" ++ lib
                                    , main]) { cwd = Just newDir
                                             , env = Just env'
                                             }

            copyFiles (testDir </> "app") appData
            copyFiles (testDir </> "files") newDir

            (ec, out, err) <- readCreateProcessWithExitCode p ""
            if ec /= ExitSuccess
                then throwIO $ TestFailure ec out err
                else return ()

removeDirRecur :: String -> IO ()
removeDirRecur path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectoryRecursive path

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
        files = zip srcFiles dstFiles

    mapM_ (createDirectoryIfMissing True) dstDirs
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
    liftM (foldl' mappend contents) $ mapM getDirContentsRecur (fst contents)

data TestFailure = TestFailure ExitCode String String

instance Show TestFailure where
    show (TestFailure ec out err) = concat
        [ "\n"
        , "\nexit code: \n" ++ show ec ++ "\n"
        , "\nstdout:    \n" ++ out
        , "\nstderr:    \n" ++ err
        ]

instance Exception TestFailure


