{-# LANGUAGE DeriveGeneric #-}

module Prescriptions.Prescription where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Receita = Receita
    { sintoma                :: String
    , medicamento_nome       :: String
    , medicamento_dosagem    :: String
    , medicamento_frequencia :: String
    , medicamento_tipo       :: String
    , medicamento_observacoes:: String
    } deriving (Show, Generic)

instance FromJSON Receita where
instance ToJSON Receita where