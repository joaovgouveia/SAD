module Appointments.AppointmentController where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import Appointments.Appointment
import Data.Maybe (fromMaybe)
import Data.List (find, any)
import Data.Time (parseTimeM, defaultTimeLocale, Day, formatTime)
import Text.Read (readMaybe)
import Users.User (User (..))

-- Converte a string recebida para o tipo data
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Verifica se um horário está na lista de horários
ehValidoHorario :: String -> [String] -> Bool
ehValidoHorario horario horarios = horario `elem` horarios

-- Verifica se a data da consulta corresponde a um dia de atendimento
ehDiaValido :: String -> [String] -> Bool
ehDiaValido dataConsulta dias = case parseDate dataConsulta of
    Just day -> let diaDaSemana = formatTime defaultTimeLocale "%A" day
                in diaDeAtendimento diaDaSemana `elem` dias
    Nothing  -> False

-- Converte uma string de dia da semana gerada para uma correspondente aos dias de atendimento possíveis no sistema
diaDeAtendimento :: String -> String
diaDeAtendimento diaDaSemana = case diaDaSemana of
    "Monday"    -> "SEGUNDA"
    "Tuesday"   -> "TERCA"
    "Wednesday" -> "QUARTA"
    "Thursday"  -> "QUINTA"
    "Friday"    -> "SEXTA"
    "Saturday"  -> "SABADO"
    "Sunday"    -> "DOMINGO"
    _           -> ""

-- Gera o ID da consulta
geraId :: String -> String -> String -> String
geraId dataConsulta horarioConsulta medicoConsulta = dataConsulta ++ "/" ++ horarioConsulta ++ "/" ++ medicoConsulta

-- Remove caracteres indesejáveis por receber uma lista como parâmetro
removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

-- Verifica se já tem uma consulta para esse horário, se existir não permite o cadastro de outra consulta
ehIdUnico :: [Consulta] -> String -> Bool
ehIdUnico consultas idConsulta = not $ any (\c -> id_consulta c == idConsulta) consultas

-- Função principal para cadastrar a consulta
writeAppointment :: FilePath -> String -> String -> String -> String -> IO String
writeAppointment pathConsultas data_consult horario medico diagnostico = do
    -- Lê o Json de usuários e cria uma lista com todos
    let pathUsers = "./Users/Users.JSON"
    profJson <- B.readFile pathUsers
    let profissionais = decode profJson :: Maybe [User]

    -- Verifica se o médico existe e é um médico
    case profissionais of
        Just ps -> case find (\p -> nome p == removeChars medico && funcao p == "MEDICO") ps of
            Just medicoInfo -> do
                -- Verifica se o horário é válido
                if not (ehValidoHorario (removeChars horario) (horarios_atendimento medicoInfo)) then
                    return "HORÁRIO INVÁLIDO OU NÃO É UM HORÁRIO DE ATENDIMENTO, HORÁRIOS NO PADRÃO HH:MM"
                else if not (ehDiaValido (removeChars data_consult) (dias_atendimento medicoInfo)) then
                    return "DATA INVÁLIDA OU NÃO É UM DIA DE ATENDIMENTO, DATA NO PADRÃO AAAA-MM-DD"
                else do
                    -- Lê o Json de consultas e cria uma lista com todas
                    consultas <- B.readFile pathConsultas
                    let consultasAntigas = decode consultas :: Maybe [Consulta]

                    -- Verifica se o ID da consulta é único
                    let idConsulta = geraId (removeChars data_consult) (removeChars horario) (removeChars medico)
                    case consultasAntigas of
                        Just cs ->
                            if ehIdUnico cs idConsulta then do
                                let novasConsultas = cs ++ [Consulta idConsulta (removeChars data_consult) (removeChars horario) (removeChars medico) (removeChars diagnostico) "Em andamento"]
                                B.writeFile pathConsultas (encode novasConsultas)
                                return "CONSULTA REGISTRADA"
                            else
                                return "JÁ EXISTE UMA CONSULTA PARA ESSE DIA E HORÁRIO"
                        Nothing -> do
                            let novasConsultas = [Consulta idConsulta (removeChars data_consult) (removeChars horario) (removeChars medico) (removeChars diagnostico) "Em andamento"]
                            B.writeFile pathConsultas (encode novasConsultas)
                            return "CONSULTA REGISTRADA"
            Nothing -> return "MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO"
        Nothing -> return "ERRO AO LER O ARQUIVO DE USUÁRIOS"


-- Verifica se o novo Status é válido
ehStatuValido :: String -> Bool
ehStatuValido a = a `elem` ["Cancelada", "Concluída"]

-- Altera o status da Consulta
updateAppointment :: FilePath -> String -> String -> IO String
updateAppointment path a b = do
    
    -- Lê o Json de consultas e converte todas para uma lista
    consultJson <- B.readFile path
    let consultas = decode consultJson :: Maybe [Consulta]

    -- Remove caracteres inválidos dos parametros
    let id_appointment = removeChars a
    let novo_status = removeChars b

    case consultas of
        Just appoint -> case find (\c -> id_consulta c == id_appointment && status_consulta c == "Em andamento") appoint of
            Just consulta -> if not (ehStatuValido novo_status) then
                return "NOVO STATUS INVÁLIDO"
            else do
                let updatedAppoints = map (\c -> if id_consulta c == id_appointment then consulta {status_consulta = novo_status} else c) appoint
                let updatedJson = encode updatedAppoints
                B.writeFile path updatedJson
                return "STATUS DA CONSULTA ATUALIZADO COM SUCESSO"
            Nothing -> return "ID DA CONSULTA INVÁLIDO/CONSULTA NÃO EXISTE OU CONSULTA JÁ FINALIZADA"
        Nothing -> return "ERRO AO LER O ARQUIVO DE CONSULTAS"