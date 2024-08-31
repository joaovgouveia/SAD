module Patients.PatitentController where

import Data.Aeson (ToJSON, FromJSON, encode, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
import Patients.Patient (Patient(..))
import Appointments.AppointmentController (viewPatientAppointment)

-- READ Todos os pacientes
viewPatients::IO String
viewPatients = do
    content <- B.readFile "./Patients/Patients.JSON"
    let patients = fromMaybe [] (decode content :: Maybe [Patient])
        result = concatMap formatPatient patients
    return result

-- READ PAciente pelo id
viewPatient :: String -> IO String
viewPatient cpfBusca = do
    let path = "./Patients/Patients.JSON"
    patients <- fromMaybe [] <$> readJsonFile path
    let filtrados = filter (\p -> id_patient p == cpfBusca) patients
    return $ formatPatient (head filtrados)


--Cria passiente e salva no JSON
createPatient :: String -> String -> IO String
createPatient cpf nome = do
    let patient = Patient (removeChars cpf) (removeChars nome) []
    savePatientToFile patient

-- Adiciona consulta ao paciente
addConsulta :: String -> String -> IO String
addConsulta cpfBusca consulta = do
    let path = "Patients/Patients.JSON"
    pacientesAntigos <- fromMaybe [] <$> readJsonFile path

    let cpfBuscaLimpo = removeChars cpfBusca
        consultaLimpo = removeChars consulta

        (atualizadas, restantes) = foldr (\p (acc, rem) ->
            let cpf = removeChars (id_patient p)
            in if cpf == cpfBuscaLimpo
                then (p { consultas = consultas p ++ [consultaLimpo] } : acc, rem)
                else (acc, p : rem)) ([], []) pacientesAntigos

    if length restantes == length pacientesAntigos
        then return "Paciente Não Encontrado."
        else do
            writeJsonFile path (atualizadas ++ restantes)
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
                  "\nConsultas: " ++ unwords (consultas p) ++ "\n"


viewPatientHistory :: String -> IO String
viewPatientHistory cpfBusca = do
    let path = "./Patients/Patients.JSON"
    patients <- fromMaybe [] <$> readJsonFile path
    let pacienteFiltrado = filter (\p -> id_patient p == cpfBusca) patients
    case pacienteFiltrado of
        [] -> return "Paciente não encontrado."
        (p:_) -> viewPatientAppointment (consultas p)

-- Utility functions
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)
