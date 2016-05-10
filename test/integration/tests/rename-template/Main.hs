import TestLib
import System.Directory

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    let prj1 = curDir ++ "/prj1"
        prj2 = curDir ++ "/prj2"

    -- Successful add templates
    setCurrentDirectory prj1
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template1"]

    setCurrentDirectory prj2
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template2"]

    -- Fail when new name already exists
    bimoFailStderrContent' ["rename", "-t", "new-template1", "-T", "new-template2"]
                           ["Template with this name already exists"]
                           "yes"

    -- Successful rename template
    bimo' ["rename", "-t", "new-template1", "-T", "new-template3"] "yes"
    bimo ["show", "-t", "new-template3"]
    bimoFail ["show", "-t", "new-template1"]


