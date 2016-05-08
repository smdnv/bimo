import TestLib
import System.Directory

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    let prj1 = curDir ++ "/prj1"
        prj2 = curDir ++ "/prj2"

    -- Fail add
    setCurrentDirectory prj1
    bimoFailAndStderrContent ["add", "-t", "new-template"]
                             [ "Not found model exec"
                             , "Build model before add"
                             ]

    -- Successful add template
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template"]
    bimoStdoutContent ["list", "-t"] ["new-template"]

    -- Fail when template already exists
    bimoFailAndStderrContent ["add", "-t", "new-template"]
                             ["Template with this name already exists"]

    -- Fail when template model already exists
    setCurrentDirectory prj2
    bimo ["build", "-p"]
    bimoFailAndStderrContent ["add", "-t", "new-template2"]
                             ["Model with this name already exists"]


