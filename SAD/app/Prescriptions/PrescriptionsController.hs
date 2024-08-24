module Prescriptions.PrescriptionsController where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as B
import Prescriptions.Prescription (Receita (..))
import Data.Maybe (fromMaybe)

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)

writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

enumerate :: IO String
enumerate = do
    sintomas <- fromMaybe [] <$> readJsonFile "./Prescriptions/SymptonsPrescriptions.JSON"
    let indexedSintomas = zip [1..] sintomas
        simpt = concatMap format indexedSintomas
    return simpt
  where
    format :: (Int, Receita) -> String
    format (index, s) = show index ++ "- " ++ sintoma s ++ "\n"


symptons :: IO [String]
symptons = do
    sintomas <- fromMaybe [] <$> readJsonFile "./Prescriptions/SymptonsPrescriptions.JSON"
    return $ map sintoma sintomas

removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

checkSymptons :: [String] -> IO Bool
checkSymptons list = do
    symptonsList <- symptons
    let cleanList = map removeChars list
    return $ all (`elem` symptonsList) cleanList

generatePrescription :: [String] -> IO String
generatePrescription symptons = do
    isSymptonsValid <- checkSymptons symptons
    if not isSymptonsValid
    then return "Sintoma(s) informado(s) não cadastrado(s)"
    else do
        prescriptions <- fromMaybe [] <$> readJsonFile "./Prescriptions/SymptonsPrescriptions.JSON"
        let prescriptionMap = [(removeChars $ sintoma sp, sp) | sp <- prescriptions]
            matchedPrescriptions = [sp | (s, sp) <- prescriptionMap, s `elem` map removeChars symptons]
            medicamentoInfo = [ "Medicamento: " ++ medicamento_nome sp
                               ++ "\nDosagem: " ++ medicamento_dosagem sp
                               ++ "\nFrequência: " ++ medicamento_frequencia sp
                               ++ "\nTipo: " ++ medicamento_tipo sp
                               ++ "\nObservações: " ++ medicamento_observacoes sp
                               ++ "\n" | sp <- matchedPrescriptions ]
            prescriptionText = "\nPrescrição:\n\n" ++ unlines medicamentoInfo
        return prescriptionText