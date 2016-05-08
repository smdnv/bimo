import TestLib
import System.Directory

main :: IO ()
main = do

    -- Config with a missing model
    renameFile "invalid_config.yaml" "config.yaml"

    bimo ["build", "-p"]
    doesExist "models/model1/exec/model1"
    doesExist "models/model2/exec/model2"

    bimoFailStderrContent ["run"] ["Not found model in config"]

    -- Model exec not exists
    renameFile "config.yaml" "invalid_config.yaml"
    renameFile "valid_config.yaml" "config.yaml"

    bimoFailStderrContent ["run"] ["Not found model exec"]

    -- Successful run
    bimo ["build", "-p"]
    bimo ["run"]
