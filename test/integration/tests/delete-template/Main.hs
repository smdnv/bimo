import TestLib
import System.Directory

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    let prj1 = curDir ++ "/prj1"
        prj2 = curDir ++ "/prj2"

    -- Add templates
    setCurrentDirectory prj1
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template1"]

    setCurrentDirectory prj2
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template2"]

    setCurrentDirectory prj1

    -- Delete template with different strategy
    --
    -- new-template1 export models:
    -- - model1
    -- - model2
    -- new-template2 export models:
    -- - model3
    -- new-template2 use model2 form new-template1
    --
    -- Delete only config.yaml, Normal flag
    bimo' ["delete", "-t", "new-template1", "-n"] "yes"
    bimoFailStderrContent ["show", "-t", "new-template1"]
                          ["Not found template"]

    -- Clear model namespace
    bimo' ["delete", "-m", "model1", "-c", "none", "-f"] "yes"
    bimo' ["delete", "-m", "model2", "-c", "none", "-f"] "yes"
    -- Add template
    bimo ["add", "-t", "new-template1"]

    -- Delete with Skip flag, delete only models without dependent templates
    -- Delete model1, but model2 still accessable
    bimo' ["delete", "-t", "new-template1", "-s"] "yes"
    bimoFail ["show", "-m", "model1", "-c", "none"]
    bimo ["show", "-m", "model2", "-c", "none"]


