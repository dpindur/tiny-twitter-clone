{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NewUser ( NewUser (..) ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data NewUser = NewUser { username :: String
                       , password :: String
                       } deriving (Show, Generic)
instance FromJSON NewUser
instance ToJSON NewUser