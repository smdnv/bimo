import TestLib
import System.Directory

main :: IO ()
main = do
    -- Successful clean
    bimo ["build"]
    bimo ["clean"]
    doesNotExist "models/model1/exec/model1"

