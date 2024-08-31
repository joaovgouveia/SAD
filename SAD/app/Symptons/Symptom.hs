{-# LANGUAGE DeriveGeneric #-}
module Symptons.Symptom where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Symptom = Symptom {
    sintoma     :: String,
    sistemas    :: [String]
} deriving (Show, Generic, Eq)

instance FromJSON Symptom where
instance ToJSON Symptom where