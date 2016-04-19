-- cli parsers there

module Bimo.Commands
    ( parser
    , Command (..))
    where

import Options.Applicative

import Bimo.New (NewOpts(..))
import Bimo.Build (BuildOpts(..))

data Command
    = New NewOpts
    | Build BuildOpts
    | Run
    | Clean
    deriving Show


new :: Parser Command
new = New <$> opts
  where
    project = NewProject
        <$> strOption (short 'p'
            <> metavar "PROJECT_NAME"
            <> help "Create new Project")
        <*> optional (strOption (short 's'
            <> metavar "SOURCE_TEMPLATE"
            <> help "Source template"))
    model = NewModel
        <$> strOption (short 'm'
            <> metavar "MODEL_NAME"
            <> help "Create new model for models lib")
        <*> optional (strOption (short 'c'
            <> metavar "MODEL_CATEGORY"
            <> help "Specify model category"))
        <*> optional (strOption (short 'l'
            <> metavar "MODEL_LANG"
            <> help "Specify model language"))
    temp = NewTemplate
        <$> strOption (short 't'
            <> metavar "TEMPLATE_NAME"
            <> help "Create new template from current project")
    opts = project <|> model <|> temp

build :: Parser Command
build = Build <$> opts
  where
    project = flag' BuildProject (short 'p' <> help "Build project")
    model   = flag' BuildModel (short 'm' <> help "Build model")
    opts    = project <|> model

run :: Parser Command
run = pure Run

clean :: Parser Command
clean = pure Clean

parser :: ParserInfo Command
parser = info (helper <*> p) idm
  where
    p = subparser
         ( command "new" (info (helper <*> new) (progDesc "Create new project"))
        <> command "build" (info (helper <*> build) (progDesc "Build project"))
        <> command "run" (info (helper <*> run) (progDesc "Run project"))
        <> command "clean" (info (helper <*> clean) (progDesc "Clean project"))
         )
