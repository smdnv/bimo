import TestLib
import System.Directory

main :: IO ()
main = do
    -- Successful build
    bimo ["build", "-p"]
    doesExist "models/model1/exec/model1"
    doesExist "models/model2/exec/model2"
    doesExist "models/model3/exec/model3"

    bimo ["run"]
