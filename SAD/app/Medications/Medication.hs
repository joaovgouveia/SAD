module Medications.Medication where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.String (fromString)

data Medication = Medication
    { nome        :: String
    , bula        :: String
    , dosagem     :: String
    } deriving (Show, Eq)

instance ToJSON Medication where
    toJSON (Medication nome bula dosagem) =
        object [ fromString "nome" .= nome
               , fromString "bula" .= bula
               , fromString "dosagem" .= dosagem
               ]
