import TestLib
-- import Control.Concurrent

main :: IO ()
main = do
    -- Successful build
    bimo ["build", "-m"]
    -- threadDelay 10000000000
    doesExist "exec/simple-model"
