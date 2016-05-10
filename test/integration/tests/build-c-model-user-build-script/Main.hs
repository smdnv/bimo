import TestLib
import System.Directory
import Control.Concurrent

main :: IO ()
main = do
    -- Successful build
    bimo ["build", "-m"]
    doesExist "exec/simple-model"

