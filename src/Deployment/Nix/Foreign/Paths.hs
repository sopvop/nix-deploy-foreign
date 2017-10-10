{-# LANGUAGE CPP #-}
module Deployment.Nix.Foreign.Paths
    ( getNixModulesPath
    )
    where

import           Control.Applicative ((<|>))

import           Data.Maybe
import           System.Environment

#ifdef NIX_DEPLOY_FOREIGN_DATA
compiledPath = "NIX_DEPLOY_FOREIGN_DATA"
#else
compiledPath = "data"
#endif

getNixModulesPath = go <$> getEnvironment
  where
    go =  fromMaybe compiledPath
          . lookup "NIX_DEPLOY_FOREIGN_DATA"

