{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude                      hiding (FilePath)

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Data.Aeson                   as J
import qualified Data.ByteString.Lazy         as LBS
import           Data.Foldable                (for_)
import qualified Data.Map                     as Map
import           Data.Semigroup               ((<>))
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text

import           Deployment.Nix.Foreign.Args
import           Deployment.Nix.Foreign.Paths
import           Deployment.Nix.Foreign.Types

import           Shelly

-- TODO: should probably link stuff to temp dir to avoid
-- parallel nix GC killing stuff.

main :: IO ()
main = do
  args <- parseCliArgs
  prog args

prog :: MonadIO m => Args -> m ()
prog args = shelly $ do
    Deployment dep <- instantiate args
    let
      machines = if null (argsMachines args)
                 then Map.keys dep
                 else argsMachines args
    for_ machines $ buildMachineEnv args
    activator <- buildActivate args
    for_ machines $ \m ->
      for_ (Map.lookup m dep) $ \d -> do
          nixCopyClosure activator d
          activateMachine activator d
    pure ()

getMkDeploy :: Sh FilePath
getMkDeploy = (</> ("mk-deploy.nix" :: FilePath) )
             <$> liftIO getNixModulesPath

instantiate :: Args -> Sh Deployment
instantiate Args{argsNixScript,argsNixPaths} = do
   echo "Instantitating deployment"
   runner <- (\p -> p </> ("mk-deploy.nix" :: FilePath) )
             <$> liftIO getNixModulesPath
   txt <- run "nix-instantiate" $ args runner
   case J.eitherDecode . LBS.fromStrict $ Text.encodeUtf8 txt of
     Left e  -> terror (fromString e)
     Right r -> pure r
  where
    args runner = pathsArgs argsNixPaths
           <> [ "--eval", "--strict", "--json"
              , "--read-write-mode"
              , "-A", "config.deploy", toTextArg runner
              , "--arg", "deploy", "import ./" <> argsNixScript]


pathsArgs :: [(Text, Text)] -> [Text]
pathsArgs = concatMap (\(k,v) -> ["-I", k <> "=" <> v])

buildMachineEnv :: Args -> Text -> Sh ()
buildMachineEnv Args{argsNixScript,argsNixPaths} machine = do
   echo $ "Building env for " <> machine
   runner <- getMkDeploy
   run_ "nix-build" $ args runner
 where
   args runner = pathsArgs argsNixPaths <>
                 [ toTextArg runner, "--arg", "deploy"
                 , "import ./" <> argsNixScript
                 , "--no-out-link"
                 , "-A", "config.deploy." <> machine <> ".env"]

buildActivate :: Args -> Sh Text
buildActivate _ = do
  p <- liftIO getActivatePath
  case p of
    Nothing -> terror "Can't find activate closure"
    Just a  -> pure (Text.pack a)

nixCopyClosure :: Text -> Machine -> Sh ()
nixCopyClosure activate m@Machine{env,name} = do
    echo $ "Copying enviroment to " <> name
    run_ "nix-copy-closure" ccargs
  where
    ccargs = ["--gzip", "--to", mkUserHost m, env, activate]

mkUserHost :: Machine -> Text
mkUserHost Machine{user,host} = user <> "@" <> host


activateMachine :: Text -> Machine -> Sh ()
activateMachine act m@Machine{env,name, profile} = do
   echo $ "Activating " <> name
   () <$ sshPairs (mkUserHost m)
         [ (fromText act </> ("bin/nix-deploy-foreign-activate"::FilePath)
                , ["activate", profile, env])]
