-- cli parsers there

module Bimo.Commands
    ( parser
    , Command (..))
    where

import Options.Applicative

import Bimo.New (NewOpts(..))
import Bimo.Build (BuildOpts(..))
import Bimo.Add (AddOpts(..))
import Bimo.List (ListOpts(..))

data Command
    = New NewOpts
    | Build BuildOpts
    | Run
    | Add AddOpts
    | List ListOpts
    | Clean
    deriving Show


new :: Parser Command
new = New <$> opts
  where
    project = NewProject
        <$> strOption (short 'p'
            <> metavar "PROJECT_NAME"
            <> help "Create new Project")
        <*> optional (strOption (short 't'
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
    opts = project <|> model

run :: Parser Command
run = pure Run

build :: Parser Command
build = Build <$> opts
  where
    project = flag' BuildProject (short 'p' <> help "Build project")
    model   = flag' BuildModel (short 'm' <> help "Build model")
    opts    = project <|> model

add :: Parser Command
add = Add <$> opts
  where
    model    = flag' AddModel (short 'm' <> help "Add model to models lib")
    template = AddTemplate
        <$> strOption (short 't'
            <> long "template"
            <> metavar "TEMPLATE_NAME"
            <> help "Create template from current project")
    opts = model <|> template

list :: Parser Command
list = List <$> opts
  where
    models = flag' ListModels (short 'm' <> help "List all lib models")
    templates = flag' ListTemplates (short 't' <> help "List all templates")
    opts = models <|> templates

clean :: Parser Command
clean = pure Clean

parser :: ParserInfo Command
parser = info (helper <*> p) idm
  where
    p = subparser
         ( command "new" (info (helper <*> new) (progDesc "Create new project"))
        <> command "build" (info (helper <*> build) (progDesc "Build project"))
        <> command "run" (info (helper <*> run) (progDesc "Run project"))
        <> command "add" (info (helper <*> add) (progDesc "Add model or template"))
        <> command "list" (info (helper <*> list) (progDesc "List models or templates"))
        <> command "clean" (info (helper <*> clean) (progDesc "Clean project"))
         )
