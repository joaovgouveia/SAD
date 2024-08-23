{-# LANGUAGE DeriveGeneric #-}

module Patients.Patient where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Patient = Patient 
    { id_patient             :: String
    , nome_patient               :: String
    } deriving (Show, Generic)

instance FromJSON Patient where
instance ToJSON Patient where