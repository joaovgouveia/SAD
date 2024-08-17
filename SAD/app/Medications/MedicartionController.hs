module Medications.MedicartionController where

import Medications.Medication
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B


-- Função para criar um Medication a partir de uma lista
createMedicationFromList :: [String] -> Medication
createMedicationFromList [name, description, dose, route] =
    Medication name description dose route
createMedicationFromList _ = error "Lista inválida. Deve conter exatamente 4 elementos."

-- Função para salvar o Medication em um arquivo
saveMedicationToFile :: Medication -> FilePath -> IO ()
saveMedicationToFile medication filePath =
    B.writeFile filePath (encode medication)