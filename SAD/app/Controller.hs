{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Controller where

import qualified Medications.MedicationController as MC
import qualified Users.UserController as US
import qualified Symptons.SymptomController as SC
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Analytics ( dashboard ) 
import qualified Diseases.DiseasesController as DC

-- Função de execução que age como ponte entre o usuário e as funcionalidades
execute :: [String] -> IO String
execute (cmd:args)
    | cmd == "addUser"         = return $ addUser args
    | cmd == "viewUser"        = return $ viewUser args
    | cmd == "deleteUser"      = return $ deleteUser args
    | cmd == "listUsers"       = return $ listUsers args
    | cmd == "addMedication"   = return $ addMedication args
    | cmd == "viewMedication"  = return $ viewMedication args
    | cmd == "listMedications" = return $ listMedications args
    | cmd == "viewMedicos"     = viewMedicos args
    | cmd == "viewMedicosAtuation"    = viewMedicosAtuation args
    | cmd == "addDisease"      = return $ addDisease args
    | cmd == "viewDisease"     = viewDisease args
    | cmd == "listDiseases"    = return $ listDiseases args
    | cmd == "addSymptom"      = return $ addSymptom args
    | cmd == "viewSymptom"     = viewSymptom args
    | cmd == "listSymptoms"    = return $ listSymptoms args
    | cmd == "viewDashBoard"   = viewDashBoard args
    | otherwise                = return "Função não existe"

-- Funções do sistema
addUser :: [String] -> String
addUser args = ""

viewUser :: [String] -> String
viewUser args = ""

deleteUser :: [String] -> String
deleteUser args = ""

listUsers :: [String] -> String
listUsers args = ""

addMedication :: [String] -> String
addMedication args =
    let medication = MC.createMedicationFromList args
    in "Medicação adicionada: " ++ show medication

viewMedication :: [String] -> String
viewMedication args =
    let medication = MC.createMedicationFromList args
    in "Medicação:" ++ show medication

listMedications :: [String] -> String
listMedications args = ""

viewMedicos :: [String] -> IO String
viewMedicos args = US.viewMedicos

viewMedicosAtuation :: [String] -> IO String
viewMedicosAtuation args = US.viewAtuation

addDisease :: [String] -> String
addDisease args = ""

viewDisease :: [String] -> IO String
viewDisease args = DC.viewDisease

listDiseases :: [String] -> String
listDiseases args = ""

addSymptom :: [String] -> String
addSymptom args = ""

viewSymptom :: [String] -> IO String
viewSymptom args = SC.viewSymptom

listSymptoms :: [String] -> String
listSymptoms args = ""

viewDashBoard :: [String] -> IO String 
viewDashBoard args = Analytics.dashboard

-- TODO: Consultas
duck :: String
duck = "quack"
