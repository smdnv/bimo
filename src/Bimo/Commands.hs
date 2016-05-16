-- cli parsers there

module Bimo.Commands
    ( parser
    , Command (..)
    )
    where

import Options.Applicative

import Bimo.Commands.New     hiding (new)
import Bimo.Commands.Build   (BuildOpts(..))
import Bimo.Commands.Add     (AddOpts(..))
import Bimo.Commands.Delete  hiding (delete)
import Bimo.Commands.Rename  (RenameOpts(..))
import Bimo.Commands.Unpack  (UnpackOpts(..))
import Bimo.Commands.List    (ListOpts(..))
import Bimo.Commands.Show    (ShowOpts(..))

data Command
    = New NewOpts
    | Build BuildOpts
    | Run
    | Add AddOpts
    | Delete DeleteOpts
    | Rename RenameOpts
    | Unpack UnpackOpts
    | List ListOpts
    | Show ShowOpts
    | Clean
    deriving Show


new :: Parser Command
new = New <$> opts
  where
    project = NewProject
        <$> strOption (short 'p'
            <> metavar "PROJECT_NAME"
            <> help "Create new Project")
        <*> optional template
    template = TemplateOpts
        <$> strOption (short 't'
            <> metavar "SOURCE_TEMPLATE"
            <> help "Source template")
        <*> switch (short 'u'
            <> long "unpack"
            <> help "Unpack models from template")
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
      opts = flag BuildProject BuildModel (short 'm' <> help "Build model")

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

delete :: Parser Command
delete = Delete <$> opts
  where
    model = DeleteModel
        <$> strOption (short 'm'
            <> metavar "MODEL_NAME"
            <> help "Model name")
        <*> strOption (short 'c'
            <> metavar "MODEL_CATEGORY"
            <> help "Model category")
        <*> flag Normal Force (short 'f'
            <> help "Force delete")
    template = DeleteTemplate
        <$> strOption (short 't'
            <> metavar "TEMPLATE_NAME"
            <> help "Delete template")
        <*> (flag' Normal (short 'n' <> help "normal add")
            <|> flag' Skip (short 's')
            <|> flag' Force (short 'f'))
    opts = model <|>template

rename :: Parser Command
rename = Rename <$> opts
  where
    template = RenameTemplate
        <$> strOption (short 't'
            <> metavar "OLD_NAME"
            <> help "Old template name")
        <*> strOption (short 'T'
            <> metavar "NEW_NAME"
            <> help "New template name")
    model = RenameModel
        <$> strOption (short 'm'
            <> metavar "OLD_NAME"
            <> help "Old model name")
        <*> strOption (short 'c'
            <> metavar "OLD_CATEGORY"
            <> help "Old model category")
        <*> strOption (short 'M'
            <> metavar "NEW_NAME"
            <> help "New model name")
        <*> strOption (short 'C'
            <> metavar "New_CATEGORY"
            <> help "New model category")
        <*> switch (short 'u'
            <> help "Update all dependent templates")
    opts = model <|> template

unpack :: Parser Command
unpack = Unpack <$> opts
  where
    opts = UnpackOpts
        <$> strOption (short 'c'
            <> long "category"
            <> metavar "CATEGORY"
            <> help "Model category")
        <*> strOption (short 'n'
            <> long "name"
            <> metavar "MODEL_NAME"
            <> help "Model name")

list :: Parser Command
list = List <$> opts
  where
    models = flag' ListModels (short 'm' <> help "List all lib models")
    templates = flag' ListTemplates (short 't' <> help "List all templates")
    opts = models <|> templates

show' :: Parser Command
show' = Show <$> opts
  where
    model = ShowModel
        <$> strOption (short 'm'
            <> long "name"
            <> metavar "MODEL_NAME"
            <> help "Model name")
        <*> strOption (short 'c'
            <> long "category"
            <> metavar "CATEGORY"
            <> help "Model category")
    template = ShowTemplate
        <$> strOption (short 't'
            <> metavar "TEMPLATE_NAME"
            <> help "Show template config")
    opts = model <|> template

clean :: Parser Command
clean = pure Clean

parser :: ParserInfo Command
parser = info (helper <*> p) idm
  where
    p = subparser
         ( command "new" (info (helper <*> new)
            (progDesc "Create new project or model"))
        <> command "build" (info (helper <*> build)
            (progDesc "Build project"))
        <> command "run" (info (helper <*> run)
            (progDesc "Run project"))
        <> command "add" (info (helper <*> add)
            (progDesc "Add model or template"))
        <> command "delete" (info (helper <*> delete)
            (progDesc "Delete model or template"))
        <> command "rename" (info (helper <*> rename)
            (progDesc "Rename model or template"))
        <> command "unpack" (info (helper <*> unpack)
            (progDesc "Unpack model"))
        <> command "list" (info (helper <*> list)
            (progDesc "List models or templates"))
        <> command "show" (info (helper <*> show')
            (progDesc "Show model or template config"))
        <> command "clean" (info (helper <*> clean)
            (progDesc "Clean project"))
         )
