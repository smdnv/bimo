import TestLib
import System.Directory

main :: IO ()
main = do
    -- Fail when not found template
    bimoFailAndStderrContent ["new", "-p", "new-project", "-t", "not-exists"]
                             ["Not found template"]

    -- Add valid template
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template"]

    -- Create new project with unpack key
    bimo ["new", "-p", "new-project", "-t", "new-template", "-u"]
    setCurrentDirectory "new-project"

    doesExist "models/model1/src"
    doesExist "models/model2/src"
    doesExist "models/model3/src"

    doesExist "models/model3/exec/model3"
    doesExist "models/model1/exec/model1"
    doesExist "models/model1/exec/model1"

    bimo ["run"]
