module Patients.PatitentController where

import Data.Maybe (fromMaybe)
import Patients.Patient (Patient(..))
import Appointments.AppointmentController (viewPatientAppointment)
import Utils.Utils (readJsonFile, removeChars, removeDuplicates, writeJsonFile)

-- READ Todos os pacientes
viewPatients::IO String
viewPatients = do
    let path = "./Patients/Patients.JSON"
    patients <- fromMaybe [] <$> readJsonFile path
    return (concatMap formatPatient patients)

-- READ PAciente pelo id
viewPatient :: String -> IO String
viewPatient cpfBusca = do
    let path = "./Patients/Patients.JSON"
    patients <- fromMaybe [] <$> readJsonFile path
    let filtrados = filter (\p -> id_patient p == cpfBusca) patients
    return $ formatPatient (head filtrados)


--Cria passiente e salva no JSON
createPatient :: String -> String -> String -> IO String
createPatient cpf name age = do
    let patient = Patient (removeChars cpf) (removeChars name) (removeChars age) []
    savePatientToFile patient

--Modifica Nome do Paciente
changePatientName :: String -> String -> IO String
changePatientName cpf newName = do
    let path = "Patients/Patients.JSON"
    pacientesAntigos <- fromMaybe [] <$> readJsonFile path

    let cleanCpf = removeChars cpf
        cleanName = removeChars newName

        (atualizadas, remaining) = foldr (\p (acc, rem) ->
            let cpf = removeChars (id_patient p)
            in if cpf == cleanCpf
        
                then (p { nome_patient = cleanName } : acc, rem)
                else (acc, p : rem)) ([], []) pacientesAntigos

    if length remaining == length pacientesAntigos
        then return "Paciente Não Encontrado."
        else do
            writeJsonFile path (atualizadas ++ remaining)
            return $ "Nome Alterado Para Paciente: " ++ formatPatient (head atualizadas)

--Modifica Nome do Paciente
deletePatient :: String -> IO String
deletePatient cpf = do
    let path = "Patients/Patients.JSON"
    pacientesAntigos <- fromMaybe [] <$> readJsonFile path

    let cleanCpf = removeChars cpf

        (removed, remaining) = foldr (\p (acc, rem) ->
            let cpf = removeChars (id_patient p)
            in if cpf == cleanCpf
                then (p : acc, rem)
                else (acc, p : rem)) ([], []) pacientesAntigos

    if null removed
        then return "Paciente Não Encontrado."
        else do
            writeJsonFile path remaining
            return $ "Paciente deletado: " ++ formatPatient (head removed)


-- Adiciona consulta ao paciente
addConsulta :: String -> String -> IO String
addConsulta cpfBusca consulta = do
    let path = "Patients/Patients.JSON"
    pacientesAntigos <- fromMaybe [] <$> readJsonFile path

    let cleanCpf = removeChars cpfBusca
        cleanConsulta = removeChars consulta

        (atualizadas, remaining) = foldr (\p (acc, rem) ->
            let cpf = removeChars (id_patient p)
            in if cpf == cleanCpf
        
                then (p { consultas = consultas p ++ [cleanConsulta] } : acc, rem)
                else (acc, p : rem)) ([], []) pacientesAntigos

    if length remaining == length pacientesAntigos
        then return "Paciente Não Encontrado."
        else do
            writeJsonFile path (atualizadas ++ remaining)
            return $ "Consulta adicionada para: " ++ formatPatient (head atualizadas)

savePatientToFile :: Patient -> IO String
savePatientToFile newPatient = do
    let pathPatients = "./Patients/Patients.JSON"

    patients <- fromMaybe [] <$> readJsonFile pathPatients

    let existe = any (\p -> id_patient p == id_patient newPatient) patients
    if existe
        then return "Paciente já existe."
        else do
            writeJsonFile pathPatients (patients ++ [newPatient])
            return $ "Paciente Cadastrado: " ++ formatPatient newPatient

-- Formata a exibição de paciente
formatPatient :: Patient -> String
formatPatient p = "\nCPF: " ++ id_patient p ++
                  "\nNome: " ++ nome_patient p ++
                  "\nIdade: " ++ idade p ++
                  "\nConsultas: " ++ unwords (consultas p) ++ "\n"


viewPatientHistory :: String -> IO String
viewPatientHistory cpfBusca = do
    let path = "./Patients/Patients.JSON"
    patients <- fromMaybe [] <$> readJsonFile path
    let pacienteFiltrado = filter (\p -> id_patient p == cpfBusca) patients
    case pacienteFiltrado of
        [] -> return "Paciente não encontrado."
        (p:_) -> viewPatientAppointment (consultas p)
