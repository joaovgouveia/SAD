{-# LANGUAGE OverloadedStrings #-}

module Analytics where

import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Users.User (User(..))
import Patients.Patient (Patient(Patient))
import Appointments.Appointment (Consulta(..))
import Data.List (group, sort, sortOn)
import Data.Ord (Down(..))

-- Formatação do usuário
doctorFormat :: User -> String
doctorFormat d = "Nome: " ++ nome d ++ "\n" ++
                     "Especialidade: " ++ especialidade d ++ "\n" ++
                     "Dias de Atendimento: " ++ unwords (dias_atendimento d) ++ "\n" ++
                     "Horários de Atendimento: " ++ unwords (horarios_atendimento d) ++ "\n" ++
                     "Pacientes atendidos: " ++ pacientes_atendidos d ++ "\n\n"

-- Retorna a quantidade de médicos
countDoctor :: [User] -> Int
countDoctor users = length $ filter (\x -> funcao x == "MEDICO") users

-- Retorna o(s) médico(s) com a maior quantidade de consultas
mostRequestedDoctor :: [User] -> String
mostRequestedDoctor users = do
    let allDoctors = filter (\x -> funcao x == "MEDICO") users
        maxConsul = maximum (map pacientes_atendidos allDoctors)
    concatMap doctorFormat [d | d <- allDoctors, pacientes_atendidos d == maxConsul]

-- DASHBOARD    
dashboard :: IO String
dashboard = do
    -- Lendo o arquivo JSON
    usersFile <- B.readFile "./Users/Users.JSON"
    patientsFile <- B.readFile "./Patients/Patients.JSON"
    
    -- Parseando o JSON
    let users = eitherDecode usersFile :: Either String [User]
    let patients = eitherDecode patientsFile :: Either String [Patient]

    -- Chama mainDiseases para calcular as principais doenças
    diseasesInfo <- mainDiseases
    
    -- Construindo o resultado em String
    return $ case users of
        Left err -> "Erro ao parsear o JSON: " ++ err
        Right userList -> do
            let qDoctor = countDoctor userList
                qPatient = length patients
                doctors = mostRequestedDoctor userList
            replicate 50 '=' ++ "\n" ++
                replicate 15 ' ' ++ "DASHBOARD\n" ++
                replicate 50 '=' ++ "\n\n" ++
                "Quantidade de médicos: " ++ 
                show qDoctor ++ "\n" ++
                "Quantidade de pacientes: " ++ 
                show qPatient ++ "\n" ++
                replicate 50 '=' ++ "\n" ++
                replicate 10 ' ' ++ "Médico(s) com mais consultas\n" ++
                replicate 50 '=' ++ "\n" ++ doctors ++ "\n" ++
                replicate 50 '=' ++ "\n" ++ 
                replicate 10 ' ' ++ "Principais doenças\n" ++
                replicate 50 '=' ++ "\n" ++ 
                diseasesInfo


-- Função para contar as doenças mais comuns
mainDiseases :: IO String
mainDiseases = do
    -- Lê o arquivo JSON de consultas
    consultasFile <- B.readFile "./Appointments/Appointments.JSON"
    
    -- Decodifica o JSON
    let consultas = eitherDecode consultasFile :: Either String [Consulta]
    
    -- Processa as consultas
    return $ case consultas of
        Left err -> "Erro ao ler consultas: " ++ err
        Right consultaList -> do
            let diseases = map diagnostico consultaList
                groupedDiseases = sortOn (Down . length) . group . sort $ diseases
                topDiseases = take 5 groupedDiseases
            unlines $ map (\grouped -> head grouped ++ " - " ++ show (length grouped) ++ " ocorrências") topDiseases

                
