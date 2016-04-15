import TestLib

main :: IO ()
main = do
    bimo ["new", "-m", "new-model"]
    doesExist "new-model/src"
    doesExist "new-model/exec"
    doesExist "new-model/model.yaml"
