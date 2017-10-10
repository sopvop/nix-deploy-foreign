{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Deployment.Nix.Foreign.Types
    ( Machine (..)
    , Deployment (..)
    ) where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Map      (Map)
import           Data.Text     (Text)


data Machine = Machine
    { host    :: Text
    , user    :: Text
    , name    :: Text
    , profile :: Text
    , env     :: Text
    } deriving (Show, Eq)

deriveJSON defaultOptions ''Machine

newtype Deployment = Deployment (Map Text Machine)
    deriving (Show, Eq, FromJSON, ToJSON)

