module Diseases.DiseasesController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
import Diseases.Disease(Disease(..))
import Utils.Utils

viewDisease :: String -> IO String
viewDisease name = do
    let path = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile path
    let found = head (filter (\p -> doenca p == name) diseases)
    return $ formatDisease found

viewDiseases :: IO String
viewDiseases = do
    content <- B.readFile "./Diseases/Diseases.JSON"
    let diseases = fromMaybe [] (decode content :: Maybe [Disease])
        result = concatMap formatDisease diseases
    return result

formatDisease::Disease -> String
formatDisease d = "Doença: " ++ doenca d ++
                  "\nPossíveis Causas: " ++ possivel_causa d ++
                  "\nEspecialidade Médica Indicada: " ++ especialidade_relacionada d ++
                  "\nMedicações Indicadas: " ++ unwords (medicamentos d) ++ "\n\n"