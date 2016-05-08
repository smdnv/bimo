import TestLib
import System.Directory

main :: IO ()
main = do
    -- Fail add
    bimoFailStderrContent ["add", "-m"] [ "Not found model exec"
                                        , "Build model before add"
                                        ]
    -- Successful add model
    bimo ["build", "-m"]
    bimo ["add", "-m"]
    bimoStdoutContent ["list", "-m"] ["simple-model"]

    -- Fail when model exist in lib
    bimoFailStderrContent ["add", "-m"] ["Model with this name already exists"]

    -- Add to another category
    renameFile "model.yaml" "old-model.yaml"
    renameFile "another-model.yaml" "model.yaml"
    bimo ["add", "-m"]
