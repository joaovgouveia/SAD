{-# LANGUAGE DeriveGeneric #-}

module Users.User where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- Definição da estrutura do médico com String
data User = User
  { id                  :: String
  , funcao              :: String
  , especialidade       :: String
  , dias_atendimento    :: [String]
  , horarios_atendimento :: [String]
  , pacientes_atendidos :: String
  , nome                :: String
  , senha               :: String
  } deriving (Show, Generic)

-- Instâncias para conversão de JSON para Json
instance FromJSON User
instance ToJSON User
