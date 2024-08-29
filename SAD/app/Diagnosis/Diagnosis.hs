{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Diagnosis.Diagnosis where

import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Maybe (fromMaybe)
import Data.Function ( on )
import qualified Data.ByteString.Lazy as B
import Utils.Utils (readJsonFile, belongs, intersect, intersectionList)
import Diseases.Disease (Disease(..))
import Medications.Medication(Medication(..))
import qualified Diseases.DiseasesController as DC

findDisease :: [String] -> IO String
findDisease symptons = do
    let path = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile path
    let filterDiseases = filter (intersect symptons . sintomas_associados) diseases
    if null filterDiseases then return "Diagnóstico incerto"
    else return (unwords (map description filterDiseases))
    where description d = DC.formatDisease d ++ "Probabilidade: " ++ show (calculateProp d) ++ "%\n\n"
          calculateProp glob = divide (length (intersectionList symptons (sintomas_associados glob))) (length (sintomas_associados glob)) * 100
          divide = (/) `on` fromIntegral