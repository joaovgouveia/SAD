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
            return $ "Medicação criada: " ++ formatMedication novaMedication

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

    let nomeBuscaLimpo = removeChars nomeBusca
        dosagemBuscaLimpa = removeChars dosagemBusca
        novaBulaLimpa = removeChars novaBula

        (atualizadas, restantes) = foldr (\med (acc, rem) ->
            let nomeMed = removeChars (nome med)
                dosagemMed = removeChars (dosagem med)
            in if nomeMed == nomeBuscaLimpo && dosagemMed == dosagemBuscaLimpa
                then (med { bula = novaBulaLimpa } : acc, rem)
                else (acc, med : rem)) ([], []) medicacoesAntigas

    if length restantes == length medicacoesAntigas
        then return "Medicação não encontrada."
        else do
            writeJsonFile pathMedications (atualizadas ++ restantes)
            return $ "Medicação atualizada para: " ++ formatMedication (head atualizadas)

-- DELETE
deleteMedication :: String -> String -> IO String
deleteMedication nomeBusca dosagemBusca = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoesAntigas <- fromMaybe [] <$> readJsonFile pathMedications

    let nomeBuscaLimpo = removeChars nomeBusca
        dosagemBuscaLimpa = removeChars dosagemBusca

        (removidos, restantes) = foldr (\med (acc, rem) ->
            let nomeMed = removeChars (nome med)
                dosagemMed = removeChars (dosagem med)
            in if nomeMed == nomeBuscaLimpo && dosagemMed == dosagemBuscaLimpa
                then (med : acc, rem)
                else (acc, med : rem)) ([], []) medicacoesAntigas

    if null removidos
        then return "Medicação não encontrada."
        else do
            writeJsonFile pathMedications restantes
            return $ "Medicação deletada: " ++ formatMedication (head removidos)

-- Função auxiliar para formatar a medicação como uma string
formatMedication :: Medication -> String
formatMedication med =
    nome med ++ ", " ++ bula med ++ ", " ++ dosagem med

-- Utility functions
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)
