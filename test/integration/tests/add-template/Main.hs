import TestLib
import System.Directory

main :: IO ()
main = do
    -- Fail add
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

