import TestLib
import System.Directory

main :: IO ()
main = do
    -- Successful build
    bimo ["build"]

    bimo ["run"]
    fileContents "out.txt" ["hello"]--
    fileContents "err.txt" ["hello"]--
