import TestLib

main :: IO ()
main = do
    bimo ["new", "-p", "new-project"]
    doesExist "new-project/models"
    doesExist "new-project/config.yaml"

