{-# LANGUAGE DeriveGeneric #-}
module Patients.Patient where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- Definição da estrutura do paciente com String
data Patient = Patient
  { id                  :: String
  , nome                :: String
  , idade               :: String
  , consultas           :: [String]
  } deriving (Show, Generic, ToJSON)

-- Instâncias para conversão de JSON para Json
instance FromJSON Patient
instance ToJSON Patient