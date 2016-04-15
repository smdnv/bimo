import TestLib
import Control.Concurrent

main :: IO ()
main = do
    bimo ["new", "-p", "new-project"]
    doesExist "new-project/config.yaml"
    doesExist "new-project/models"

