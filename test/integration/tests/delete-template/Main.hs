import TestLib
import System.Directory

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    let prj1 = curDir ++ "/prj1"
        prj2 = curDir ++ "/prj2"

    -- Add templates
    -- new-template2 user lib models from new-template1
    setCurrentDirectory prj1
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template1"]

    setCurrentDirectory prj2
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template2"]

    setCurrentDirectory curDir

    -- Delete template with different strategy
    -- Delete only config.yaml, Normal flag
    bimo ["delete", "-t", "new-template1", "-n"]
    bimoFailAndStderrContent ["show", "-t", "new-template1"]
                             ["Not found template"]



