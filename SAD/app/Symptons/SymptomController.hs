module Symptons.SymptomController where

import Data.Maybe (fromMaybe)
import Symptons.Symptom (Symptom(..))
import Utils.Utils (readJsonFile, writeJsonFile)

pathSymptoms :: String
pathSymptoms = "./Symptons/Symptoms.JSON"

viewSymptoms :: IO String
viewSymptoms = do
    symptoms <- fromMaybe [] <$> readJsonFile pathSymptoms
    return (unwords (map formatSymptom symptoms))

viewSymptomsByArea :: String -> IO String
viewSymptomsByArea area = do
    symptoms <- fromMaybe [] <$> readJsonFile pathSymptoms
    let filteredSymptoms = filter (\x -> area `elem` sistemas x) symptoms
    if null filteredSymptoms then return "Sistema Inexistente."
    else return (unwords (map formatSymptom filteredSymptoms))

formatSymptom :: Symptom -> String
formatSymptom s = sintoma s ++ " - Sistemas afetados: " ++ unwords (sistemas s)  ++ "\n"