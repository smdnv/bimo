import TestLib

main :: IO ()
main = do
    -- Successful build
    bimo ["build", "-m"]
    doesExist "exec/simple-model"
