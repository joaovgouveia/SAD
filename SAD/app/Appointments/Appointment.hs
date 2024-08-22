{-# LANGUAGE DeriveGeneric #-}

module Appointments.Appointment where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Consulta = Consulta 
    { id_consulta             :: String
    , data_consulta           :: String
    , horario_consulta        :: String
    , medico_responsavel      :: String
    , diagnostico             :: String
    , status_consulta         :: String
    } deriving (Show, Generic)

instance FromJSON Consulta where
instance ToJSON Consulta where