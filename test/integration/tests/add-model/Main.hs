import TestLib

main :: IO ()
main = do
    -- Fail add
    bimoFailAndStderrContent ["add", "-m"] [ "Not found model exec"
                                           , "Build model before add"
                                           ]
    -- Successful build
    bimo ["build", "-m"]
    bimo ["add", "-m"]
    bimoStdoutContent ["list", "-m"] ["simple-model"]
