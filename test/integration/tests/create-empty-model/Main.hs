import TestLib

main :: IO ()
main = do
    -- fail if dir exists
    bimoFail ["new", "-m", "exists-dir"]
    -- create empty model
    bimo ["new", "-m", "new-model"]
    doesExist "new-model/src"
    doesExist "new-model/exec"
    doesExist "new-model/model.yaml"
    fileContents "new-model/model.yaml" ["category: none"]
    -- create model, specify language and category
    bimo [ "new", "-m", "new-model2", "-l", "haskell", "-c", "coder"]
    fileContents "new-model2/model.yaml" [ "category: coder"
                                         , "language: haskell"
                                         ]
