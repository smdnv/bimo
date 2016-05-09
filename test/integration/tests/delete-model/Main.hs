import TestLib
import System.Directory

main :: IO ()
main = do
    -- Add templates
    bimo ["build", "-p"]
    bimo ["add", "-t", "new-template1"]

    -- Delete model which is used in a template

    -- Delete with Normal flag (default)
    -- Fail when exists dependency template
    bimoFailStderrContent' ["delete", "-m", "model1", "-c", "none"]
                           ["Existing dependence with"]
                           "yes"
    -- Delete template
    bimo' ["delete", "-t", "new-template1", "-n"] "yes"
    -- Successful delete models
    bimo' ["delete", "-m", "model1", "-c", "none"] "yes"
    bimoFail ["show", "-m", "model1", "-c", "none"]

    -- Delete with Force flag
    bimo' ["delete", "-m", "model2", "-c", "none", "-f"] "yes"
    bimoFail ["show", "-m", "model2", "-c", "none"]




