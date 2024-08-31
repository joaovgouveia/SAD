{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Controller where

import qualified Medications.MedicationController as MC
import qualified Users.UserController as UC
import qualified Patients.PatitentController as PC
import qualified Symptons.SymptomController as SC
import qualified Diseases.DiseasesController as DC
import qualified Appointments.AppointmentController as AC
import qualified Prescriptions.PrescriptionsController as PRE
import qualified Diagnosis.Diagnosis as D
import Analytics ( dashboard )

-- Função de execução que age como ponte entre o usuário e as funcionalidades
execute :: [String] -> IO String
execute (cmd:args)
    | cmd == "verUser"                      = viewUser args
    | cmd == "listaUsers"                   = viewUsers args
    | cmd == "listaUsersPorFuncao"          = viewUsersByFunction args
    | cmd == "addMed"                       = addMedication args
    | cmd == "verMed"                       = viewMedication args
    | cmd == "listaMeds"                    = listMedications args
    | cmd == "mudaMed"                      = updateMedication args
    | cmd == "deletaMed"                    = deleteMedication args
    | cmd == "listaSintomas"                = viewSymptoms args
    | cmd == "listaSintomasSistema"         = viewSymptomsByArea args
    | cmd == "verDoenca"                    = viewDisease args
    | cmd == "listaDoencas"                 = listDiseases args
    | cmd == "gerarPrescricao"              = generatePrescrip args
    | cmd == "listarSintomasPresc"          = enumerateSymptons
    | cmd == "Dashboard"                    = viewDashBoard args
    | cmd == "addPaciente"                  = addPatient args
    | cmd == "verPaciente"                  = viewPatient args
    | cmd == "verPacientes"                 = viewPatients args
    | cmd == "mudaNomePaciente"             = changePatientName args
    | cmd == "deletaPaciente"               = deletePatient args
    | cmd == "verHistóricoPaciente"         = viewPatientHistory args
    | cmd == "addConsulta"                  = addAppointment args
    | cmd == "mudaStatusConsulta"           = changeStatusAppointment args
    | cmd == "verConsultasDisponiveis"      = viewAvailableAppointment args
    | cmd == "verEspecialidadesMedicas"     = viewMedicosAtuation args
    | cmd == "listaMedicos"                 = viewMedicos args
    | cmd == "diagnosticar"                 = diagnosticar args
    | otherwise                             = return "Função não existe"

-- Funções do sistema

-- User
viewUser :: [String] -> IO String
viewUser [] = return "Necessário ID para exibir Usuário."
viewUser args = UC.viewUser (head args)

viewUsers :: [String] -> IO String
viewUsers args = UC.viewUsers

viewUsersByFunction :: [String] -> IO String
viewUsersByFunction [] = return "Necessário Função para exibir os Usuários."
viewUsersByFunction args = UC.viewUsersByFunction (head args)

-- Patient
viewPatients :: [String] -> IO String
viewPatients args = PC.viewPatients

viewPatient :: [String] -> IO String
viewPatient [] = return "Necessário CPF para exibir Paciente."
viewPatient args = PC.viewPatient (head args)

addPatient :: [String] -> IO String
addPatient [cpf, name, age] = PC.createPatient cpf name age
addPatient _ = return "Nessessário CPF, Nome e Idade do Paciente para cadastra-lo."

changePatientName :: [String] -> IO String
changePatientName [cpf, name] = PC.changePatientName cpf name
changePatientName _ = return "Nessessário CPF e um Novo nome para mudar o nome de um Paciente."

deletePatient :: [String] -> IO String
deletePatient [] = return "Nessessário CPF do Paciente para deleta-lo."
deletePatient args = PC.deletePatient (head args)

viewPatientHistory :: [String] -> IO String
viewPatientHistory [] = return "Id inválido"
viewPatientHistory (id:s) = PC.viewPatientHistory id

-- Medication
addMedication :: [String] -> IO String
addMedication [a, b, c] = MC.createMedication a b c
addMedication _ = return "Necessário Nome, Bula e Dosagem para cadastro da Medicação."

viewMedication :: [String] -> IO String
viewMedication args = MC.viewMedication (head args)

listMedications :: [String] -> IO String
listMedications args = MC.readMedications

updateMedication :: [String] -> IO String
updateMedication [a, b, c] = MC.updateMedication a b c

deleteMedication :: [String] -> IO String
deleteMedication [a, b] = MC.deleteMedication a b

--Appointment
viewAvailableAppointment :: [String] -> IO String
viewAvailableAppointment [a] = AC.checkSchedule [a]
viewAvailableAppointment _ = return "Necessário exatamente 1 informação para verificar horários"

addAppointment :: [String] -> IO String
addAppointment [a, b, c, d, e] = AC.writeAppointment a b c d e
addAppointment _ = return "Necessário exatamente 5 informações para cadastro da Consulta."

changeStatusAppointment :: [String] -> IO String
changeStatusAppointment [a, b] = AC.updateAppointment a b
changeStatusAppointment _ = return "Necessário exatamente 2 informações para atualização de status da Consulta."

-- Symptoms
enumerateSymptons :: IO String
enumerateSymptons = PRE.enumerate

viewSymptoms :: [String] -> IO String
viewSymptoms args = SC.viewSymptoms

viewSymptomsByArea :: [String] -> IO String
viewSymptomsByArea [] = return "Necessário um Sistema para listar os Sintomas qe o afetam."
viewSymptomsByArea args = SC.viewSymptomsByArea (head args)

-- Disease
viewDisease :: [String] -> IO String
viewDisease args = DC.viewDisease (head args)

listDiseases :: [String] -> IO String
listDiseases args = DC.viewDiseases

-- Médico
viewMedicos :: [String] -> IO String
viewMedicos args = UC.viewMedicos

viewMedicosAtuation :: [String] -> IO String
viewMedicosAtuation args = UC.viewAtuation

-- Outros
generatePrescrip :: [String] -> IO String
generatePrescrip a = if length a > 10 then
    return "Necessário respeitar o limite cadastrado no sistema, até 10 sintomas." else PRE.generatePrescription a

viewDashBoard :: [String] -> IO String
viewDashBoard args = Analytics.dashboard

diagnosticar :: [String] -> IO String
diagnosticar = D.findDisease

-- Nothing to see here
duck :: String
duck = "quack"
