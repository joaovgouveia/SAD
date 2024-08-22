module Medications.MedicationController where

import Medications.Medication
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Medications.Medication (Medication (..))
import Appointments.AppointmentController as AC


-- Função para criar um Medication a partir de uma lista
createMedication :: [String] -> IO String
createMedication [name, bula, dosagem] = do
    let medication = Medication name bula dosagem
    saveMedicationToFile medication
createMedicationFromList _ = error "Lista inválida. Deve conter exatamente 3 elementos."

-- Função para salvar o Medication em um arquivo
saveMedicationToFile :: Medication -> IO String
saveMedicationToFile medication = do
    let pathMedications = "./Medications/Medications.JSON"
    medicacoesAntigas <- fromMaybe [] <$> readJsonFile pathMedications
    let existe = any (\m -> name m == name medication && dosagem m == dosagem medication) medicacoesAntigas
    if not existe
        then do
            writeJsonFile pathMedications (medicacoesAntigas ++ [medication])
            return $ "Medicação criada: " ++ show medication
        else return "Medicação existente"
