module Medications.MedicationController where

import Medications.Medication
import Data.Maybe (fromMaybe)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as B

createMedication :: String -> String -> String -> IO String
createMedication nome bula dosagem = do
    let medication = Medication (removeChars nome) (removeChars bula) (removeChars dosagem)
    saveMedicationToFile medication

saveMedicationToFile :: Medication -> IO String
saveMedicationToFile novaMedication = do
    let pathMedications = "./Medications/Medications.JSON"

    medicacoesAntigas <- fromMaybe [] <$> readJsonFile pathMedications

    let existe = any (\m -> nome m == nome novaMedication && dosagem m == dosagem novaMedication) medicacoesAntigas
    if existe
        then return "Medicação já existe."
        else do
            writeJsonFile pathMedications (medicacoesAntigas ++ [novaMedication])
            return $ "Medicação criada: " ++ show novaMedication

writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)
