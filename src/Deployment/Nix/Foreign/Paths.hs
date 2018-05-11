module Deployment.Nix.Foreign.Paths
    ( getNixModulesPath
    , getActivatePath
    )
    where

import           Data.Maybe
import           System.Environment

getNixModulesPath :: IO String
getNixModulesPath = go <$> getEnvironment
  where
    go =  fromMaybe "data"
          . lookup "NIX_DEPLOY_FOREIGN_DATA"

getActivatePath :: IO (Maybe String)
getActivatePath = go <$> getEnvironment
  where
    go = lookup "NIX_DEPLOY_FOREIGN_ACTIVATE"

