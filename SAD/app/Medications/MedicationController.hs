module Medications.MedicationController where

import Medications.Medication
import Data.Maybe (fromMaybe)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as B

-- CREATE
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

-- READ (nome e dosagem)
readMedications :: IO String
readMedications = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoes <- fromMaybe [] <$> readJsonFile pathMedications
    return $ formatMedications medicacoes

formatMedications :: [Medication] -> String
formatMedications = unlines . map (\m -> nome m ++ " - " ++ dosagem m)

-- READ (por nome, incluindo a bula)
viewMedication :: String -> IO String
viewMedication nomeBusca = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoes <- fromMaybe [] <$> readJsonFile pathMedications
    let filtrados = filter (\m -> nome m == nomeBusca) medicacoes
    return $ formatDetailedMedications filtrados

formatDetailedMedications :: [Medication] -> String
formatDetailedMedications = unlines . map (\m -> nome m ++ " - " ++ dosagem m ++ " - " ++ bula m)

-- UPDATE
updateMedication :: String -> String -> String -> IO String
updateMedication nomeBusca novaBula dosagemBusca = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoesAntigas <- fromMaybe [] <$> readJsonFile pathMedications

    let (atualizadas, restantes) = foldr (\med (acc, rem) ->
            if nome med == (removeChars nomeBusca) && dosagem med == removeChars(dosagemBusca)
                then (med { bula = (removeChars novaBula) } : acc, rem)
                else (acc, med : rem)) ([], []) medicacoesAntigas

    if length restantes == length medicacoesAntigas
        then return "Medicação não encontrada."
        else do
            writeJsonFile pathMedications (atualizadas ++ restantes)
            return $ "Medicação atualizada para: " ++ show (head atualizadas)

-- DELETE
deleteMedication :: String -> String -> IO String
deleteMedication nomeBusca dosagemBusca = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoesAntigas <- fromMaybe [] <$> readJsonFile pathMedications

    let (removidos, restantes) = foldr (\med (acc, rem) ->
            if nome med == nomeBusca && dosagem med == dosagemBusca
                then (med : acc, rem)
                else (acc, med : rem)) ([], []) medicacoesAntigas

    if null removidos
        then return "Medicação não encontrada."
        else do
            writeJsonFile pathMedications restantes
            return $ "Medicação deletada: " ++ show (head removidos)

-- Utility functions
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)
