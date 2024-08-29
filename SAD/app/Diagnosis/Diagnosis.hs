module Diagnosis.Diagnosis where

import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Diseases.Disease (Disease(..))
import Medications.Medication(Medication(..))
import Prescriptions.PrescriptionsController (symptons)
import Diseases.DiseasesController(formatDisease)

-- (sintomasAssociados (head diseases))
findDisease :: [String] -> IO String
findDisease symptons = do
    putStrLn (unwords symptons)
    let path = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile path
    putStrLn (unwords (map Diseases.DiseasesController.formatDisease diseases))
    let filterDiseases = filter (belongs symptons . sintomasAssociados) diseases
    return (unwords (map Diseases.DiseasesController.formatDisease filterDiseases))

-- Utility functions
readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)

belongs :: Eq a => [a] -> [a] -> Bool
belongs (h:t) l
    | null t && h `elem` l = True
    | h `elem` l = belongs t l
    | otherwise = False