module CLIOptions (CLIOptions (..), parse) where

import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, showDefault, strOption, value, (<**>))

data CLIOptions = CLIOptions
  { configPath :: FilePath,
    jwkPath :: FilePath
  }

parse :: IO CLIOptions
parse =
  execParser $ info (inputOptionsParser <**> helper) fullDesc

inputOptionsParser :: Parser CLIOptions
inputOptionsParser =
  CLIOptions
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> help "Path for the file containing the application configuration"
          <> showDefault
          <> value "./config.toml"
      )
    <*> strOption
      ( long "jwk"
          <> metavar "JWK"
          <> help "Path for the file storing the authentication key"
          <> showDefault
          <> value "./.jwk"
      )
