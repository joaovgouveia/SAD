module Diseases.DiseasesController where

import Data.Maybe (fromMaybe)
import Diseases.Disease(Disease(..))
import Utils.Utils (readJsonFile)

viewDisease :: String -> IO String
viewDisease name = do
    let path = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile path
    let found = head (filter (\p -> doenca p == name) diseases)
    return $ formatDisease found

viewDiseases :: IO String
viewDiseases = do
    let path = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile path
    return (concatMap formatDisease diseases)

formatDisease::Disease -> String
formatDisease d = "Doença: " ++ doenca d ++
                  "\nPossíveis Causas: " ++ possivel_causa d ++
                  "\nEspecialidade Médica Indicada: " ++ especialidade_relacionada d ++
                  "\nMedicações Indicadas: " ++ unwords (medicamentos d) ++ "\n\n"