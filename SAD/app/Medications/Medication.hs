{-# LANGUAGE DeriveGeneric #-}

module Medications.Medication where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Medication = Medication
    { nome        :: String
    , bula        :: String
    , dosagem     :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Medication
instance ToJSON Medication
