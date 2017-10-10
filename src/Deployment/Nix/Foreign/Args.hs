module Deployment.Nix.Foreign.Args
    ( Args (..)
    , parseCliArgs
    ) where

import           Data.Semigroup      ((<>))
import           Data.String         (fromString)
import           Data.Text           (Text)

import           Options.Applicative (Parser, execParser, help, helper, info,
                                      long, many, maybeReader, metavar, option,
                                      short, strOption, value)

data Args = Args
  { argsNixScript :: Text
  , argsNixPaths  :: [(Text, Text)]
  , argsMachines  :: [Text]
  } deriving (Eq, Ord, Show)


nixScriptOpt :: Parser Text
nixScriptOpt =  fmap fromString . strOption $
   value "deploy.nix"
   <> metavar "FILENAME"
   <> help "Deployment description, ./deploy.nix by default."
   <> short 'c'
   <> long "config"

includeOpt :: Parser [(Text, Text)]
includeOpt = many . option (maybeReader parseIt) $
  metavar "name=PATH"
  <> help "Extra paths to pass to nix"
  <> short 'I'
  where
    parseIt = go id
    go _ []         = Nothing
    go acc ('=':xs) = Just (fromString (acc []), fromString xs)
    go acc (x:xs)   = go ((++[x]) . acc) xs

machineOpt :: Parser [Text]
machineOpt = many . fmap fromString . strOption $
  metavar "NAME"
  <> help "Only deploy to specified machines"
  <> short 'm'
  <> long "machine"

parseArgs :: Parser Args
parseArgs = Args <$> nixScriptOpt
                 <*> includeOpt
                 <*> machineOpt

parseCliArgs :: IO Args
parseCliArgs = execParser $ info (helper <*> parseArgs) mempty
