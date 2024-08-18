module Medications.Medication where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.String (fromString)

data Medication = Medication
    { medName        :: String
    , medDescription :: String
    , medDose        :: String
    , medRoute       :: String
    } deriving (Show, Eq)

instance ToJSON Medication where
    toJSON (Medication medName medDescription medDose medRoute) =
        object [ fromString "name" .= medName
               , fromString "description" .= medDescription
               , fromString "dose" .= medDose
               , fromString "route" .= medRoute
               ]
