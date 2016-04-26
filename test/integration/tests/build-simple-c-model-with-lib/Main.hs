import TestLib
import Control.Concurrent

main :: IO ()
main = do
    -- Successful build
    -- threadDelay 10000000000
    bimo ["build", "-m"]
    doesExist "exec/simple-model"
