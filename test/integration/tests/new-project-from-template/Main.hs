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

    -- Create new project and run
    bimo ["new", "-p", "new-project", "-t", "new-template"]
    setCurrentDirectory "new-project"
    bimo ["run"]

