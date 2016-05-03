import TestLib

main :: IO ()
main = do
    bimoFail ["new", "-p", "exists-dir"]
    bimo ["new", "-p", "new-project"]
    doesExist "new-project/models"
    doesExist "new-project/config.yaml"

