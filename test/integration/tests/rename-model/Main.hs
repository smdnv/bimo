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
    bimoFailStderrContent'
        ["rename", "-m", "model1", "-c", "none", "-M", "model2", "-C", "none"]
        ["Model with this name already exists"]
        "yes"

    -- Fail when exists dependency template
    bimoFailStderrContent'
        ["rename", "-m", "model1", "-c", "none", "-M", "new-model1", "-C", "none"]
        [ "Existing dependence with"
        , "new-template1"
        ]
        "yes"

    -- Rename model without dependency
    bimo' ["delete", "-t", "new-template1", "-n"] "yes"
    bimo' ["rename", "-m", "model1", "-c", "none", "-M", "new-model1", "-C", "none"]
          "yes"

    -- Successful rename model and update template
    bimo' ["rename", "-m", "model2", "-c", "none", "-M", "new-model2", "-C", "none", "-u"]
          "yes"
    bimoStdoutContent ["show", "-t", "new-template2"]
                      ["new-model2"]


