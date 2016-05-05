import TestLib
import System.Directory

main :: IO ()
main = do
    -- Add model
    bimo ["build", "-m"]
    bimo ["add", "-m"]

    -- Unpack model
    bimo ["unpack", "-c", "none", "-n", "simple-model"]
    doesExist "simple-model"
    setCurrentDirectory "simple-model"

    -- Successful build unpacked model
    bimo ["build", "-m"]
