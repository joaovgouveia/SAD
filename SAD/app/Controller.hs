{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Controller where

import qualified Medications.MedicationController as MC
import qualified Users.UserController as UC
import qualified Patients.PatitentController as PC
import qualified Symptons.SymptomController as SC
import qualified Diseases.DiseasesController as DC
import qualified Appointments.AppointmentController as AC
import Prescriptions.PrescriptionsController as PRE
import qualified Diagnosis.Diagnosis as D 
import Analytics ( dashboard )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
 
-- Função de execução que age como ponte entre o usuário e as funcionalidades
execute :: [String] -> IO String
execute (cmd:args)

    | cmd == "viewUser"        = return $ viewUser args
    | cmd == "listUsers"       = return $ listUsers args
    | cmd == "addMedication"   = addMedication args
    | cmd == "viewMedication"  = viewMedication args
    | cmd == "listMedications" = listMedications args
    | cmd == "updateMedication" = updateMedication args
    | cmd == "deleteMedication" = deleteMedication args
    | cmd == "viewMedicos"     = viewMedicos args
    | cmd == "viewMedicosAtuation" = viewMedicosAtuation args
    | cmd == "addAppointment"  = addAppointment args
    | cmd == "changeStatusAppointment" = changeStatusAppointment args
    | cmd == "enumerateSymptons"        = enumerateSymptons
    | cmd == "generatePrescription"     = generatePrescrip args
    | cmd == "addDisease"      = return $ addDisease args
    | cmd == "viewDisease"     = viewDisease args
    | cmd == "listDiseases"    = return $ listDiseases args
    | cmd == "addSymptom"      = return $ addSymptom args
    | cmd == "viewSymptom"     = viewSymptom args
    | cmd == "listSymptoms"    = return $ listSymptoms args
    | cmd == "viewDashBoard"   = viewDashBoard args
    | cmd == "addPatient"      = addPatient args
    | cmd == "viewPatient"     = viewPatient args
    | cmd == "viewPatients"    = viewPatients args
    | cmd == "addPatient"      = addPatient args
    | cmd == "diagnosticar"             = diagnosticar args
    | cmd == "viewPatientHistory"       = viewPatientHistory args
    | cmd == "viewAvailableAppointment" = viewAvailableAppointment args
    | otherwise                = return "Função não existe"

viewAvailableAppointment :: [String] -> IO String
viewAvailableAppointment args = AP.checkSchedule args
-- Funções do sistema
viewUser :: [String] -> String
viewUser args = ""

listUsers :: [String] -> String
listUsers args = ""

addMedication :: [String] -> IO String
addMedication [a, b, c] = MC.createMedication a b c
addMedication _ = return "Necessário exatamente 3 informações para cadastro da Medicação"

viewMedication :: [String] -> IO String
viewMedication [a] = MC.viewMedication a

listMedications :: [String] -> IO String
listMedications args = MC.readMedications

updateMedication :: [String] -> IO String
updateMedication [a, b, c] = MC.updateMedication a b c

deleteMedication :: [String] -> IO String
deleteMedication [a, b] = MC.deleteMedication a b

viewMedicos :: [String] -> IO String
viewMedicos args = UC.viewMedicos

viewMedicosAtuation :: [String] -> IO String
viewMedicosAtuation args = UC.viewAtuation

addAppointment :: [String] -> IO String
addAppointment [a, b, c, d, e] = AC.writeAppointment a b c d e
addAppointment _ = return "Necessário exatamente 5 informações para cadastro da Consulta"

changeStatusAppointment :: [String] -> IO String
changeStatusAppointment [a, b] = AC.updateAppointment a b
changeStatusAppointment _ = return "Necessário exatamente 2 informações para atualização de status da Consulta"

enumerateSymptons :: IO String
enumerateSymptons = PRE.enumerate

generatePrescrip :: [String] -> IO String
generatePrescrip a = if length a > 10 then
    return "Necessário respeitar o limite cadastrado no sistema, até 10 sintomas" else PRE.generatePrescription a

viewDisease :: [String] -> IO String
viewDisease args = DC.viewDisease (head args)

listDiseases :: [String] -> IO String
listDiseases args = DC.viewDiseases

viewSymptom :: [String] -> IO String
viewSymptom args = SC.viewSymptom

listSymptoms :: [String] -> String
listSymptoms args = ""

viewDashBoard :: [String] -> IO String
viewDashBoard args = Analytics.dashboard

viewPatients :: [String] -> IO String
viewPatients args = PC.viewPatients

viewPatient :: [String] -> IO String
viewPatient args = PC.viewPatient (head args)

addPatient :: [String] -> IO String
addPatient [cpf, name] = PC.createPatient cpf name
addPatient _ = return "Nessessário CPF e Nome do Paciente para cadastra-lo"

diagnosticar :: [String] -> IO String
diagnosticar = D.findDisease

viewPatientHistory :: [String] -> IO String
viewPatientHistory [] = return "Id inválido"  
viewPatientHistory (id:s) = PC.viewPatientHistory id

-- Nothing to see here
duck :: String
duck = "quack"