import TestLib
import System.Directory
import Control.Concurrent

main :: IO ()
main = do
    -- Fail when not found model config
    renameFile "model.yaml" "hide-model.yaml"
    bimoFailStderrContent ["build", "-m"] ["Not found model config"]

    -- Fail when build model
    renameFile "hide-model.yaml" "model.yaml"
    renameFile "src/simple-model.c" "src/hide-simple-model.c"
    bimoFailStderrContent ["build", "-m"] ["Failure when build model"]

    -- Successful build
    renameFile "src/hide-simple-model.c" "src/simple-model.c"
    bimo ["build", "-m"]
    doesExist "exec/simple-model"

    -- Not build if not necessary
    before <- getModificationTime "exec/simple-model"
    bimo ["build", "-m"]
    after <- getModificationTime "exec/simple-model"
    if (before == after)
        then return ()
        else error "time not eq"

