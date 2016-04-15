module Bimo
    (run)
    where

import Options.Applicative
import Control.Monad.Reader
import Control.Monad.Logger
import Path
import Path.IO

import Bimo.Types.Env
import Bimo.Commands
import Bimo.New

run :: IO ()
run = do
  env <- Env <$> getAppUserDataDir "bimo"
             <*> parseRelFile "config.yaml"
             <*> parseRelDir "models"
             <*> parseRelDir ".exec"
             <*> parseRelDir "models"
             <*> parseRelFile "models.yaml"
             <*> parseRelDir "templates"
  args <- execParser parser
  case args of
      New opts -> runStdoutLoggingT $ runReaderT (new opts) env
      _ -> print args


-- modelCmd :: ...
