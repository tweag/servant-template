module InputOptions where

-- optparse-applicative
import Options.Applicative (Parser, strOption, long, metavar, help, showDefault, value)

data InputOptions = InputOptions
  { configPath :: FilePath
  , jwkPath    :: FilePath
  }

inputOptionsParser :: Parser InputOptions
inputOptionsParser = InputOptions
  <$> strOption
    (  long "config"
    <> metavar "CONFIG"
    <> help "Path for the file containing the application configuration"
    <> showDefault
    <> value "./config.toml" )
  <*> strOption
    (  long "jwk"
    <> metavar "JWK"
    <> help "Path for the file storing the authentication key"
    <> showDefault
    <> value "./.jwk" )
