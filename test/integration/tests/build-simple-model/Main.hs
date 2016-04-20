import TestLib

main :: IO ()
main = do
    bimo ["build", "-m"]
    doesExist "exec/simple-model"
