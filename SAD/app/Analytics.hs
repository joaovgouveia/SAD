{-# LANGUAGE OverloadedStrings #-}

module Analytics where

import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Users.User (User(..))

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

-- Retorna o número de pacientes
countPatient :: [User] -> Int
countPatient users = length $ filter (\x -> funcao x == "PACIENTE") users

-- Retorna o(s) médico(s) com a maior quantidade de consultas
mostRequestedDoctor :: [User] -> String
mostRequestedDoctor users = do
    let allDoctors = filter (\x -> funcao x == "MEDICO") users
        maxConsul = maximum (map pacientes_atendidos allDoctors)
    concatMap doctorFormat [d | d <- allDoctors, pacientes_atendidos d == maxConsul]

dashboard :: IO String
dashboard = do
    -- Lendo o arquivo JSON
    conteudo <- B.readFile "./Users/Users.JSON"
    
    -- Parseando o JSON
    let users = eitherDecode conteudo :: Either String [User]

    -- Construindo o resultado em String
    return $ case users of
        Left err -> "Erro ao parsear o JSON: " ++ err
        Right userList -> do
            let qDoctor = countDoctor userList
                qPatient = countPatient userList
                doctors = mostRequestedDoctor userList
            replicate 50 '=' ++ "\n" ++
                replicate 15 ' ' ++ "DASHBOARD\n" ++
                replicate 50 '=' ++ "\n\n" ++
                "Quantidade de médicos: " ++ 
                show qDoctor ++ "\n\n" ++
                "Quantidade de pacientes: " ++ 
                show qPatient ++ "\n\n" ++
                replicate 50 '=' ++ "\n" ++
                replicate 10 ' ' ++ "Médico(s) com mais consultas\n" ++
                replicate 50 '=' ++ "\n" ++ doctors
