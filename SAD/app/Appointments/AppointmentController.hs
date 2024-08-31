{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Appointments.AppointmentController where

import Appointments.Appointment
import Data.Maybe (fromMaybe)
import Data.List (find, any)
import Data.Time (parseTimeM, defaultTimeLocale, Day, formatTime)
import Utils.Utils (removeChars, readJsonFile, writeJsonFile)
import Users.User (User (..))
import Patients.Patient (Patient (..))
import qualified Control.Arrow as Data.Bifunctor

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

-- Verifica se já tem uma consulta para esse horário, se existir não permite o cadastro de outra consulta
ehIdUnico :: [Consulta] -> String -> Bool
ehIdUnico consultas idConsulta = not $ any (\c -> id_consulta c == idConsulta) consultas

-- Verifica se o paciente existe no sistema
ehPacienteValido :: String -> IO Bool
ehPacienteValido idPaciente = do
    pacientes <- fromMaybe [] <$> readJsonFile "./Patients/Patients.JSON"
    return $ any (\p -> id_patient p == idPaciente) pacientes

-- Funcão para atualizar a quantidade de consultas do médico
atualizaAtendimentos :: String -> IO String
atualizaAtendimentos medico = do
    usuarios <- fromMaybe [] <$> readJsonFile "./Users/Users.JSON"
    case find (\p -> nome p == removeChars medico && funcao p == "MEDICO") usuarios of
        Just doctor -> do
            let updateConsultas = map (\u -> if nome u == removeChars medico then doctor {pacientes_atendidos = show ((read (pacientes_atendidos u) :: Int) + 1)} else u) usuarios
            writeJsonFile "./Users/Users.JSON" updateConsultas
            return ""
        Nothing -> return "MÉDICO DESAPARECEU EM TEMPO DE EXECUÇÃO"

-- Função principal para cadastrar a consulta
writeAppointment :: String -> String -> String -> String -> String -> IO String
writeAppointment data_consult horario medico diagnostico idPaciente = do
    -- Lê o JSON de usuários e cria uma lista com todos
    profissionais <- fromMaybe [] <$> readJsonFile "./Users/Users.JSON"
    let pathConsultas = "./Appointments/Appointments.JSON"

    -- Verifica se o médico existe e é um médico
    case find (\p -> nome p == removeChars medico && funcao p == "MEDICO") profissionais of
        Just medicoInfo -> do
            -- Verifica se o horário e a data são válidos
            let horarioLimpo = removeChars horario
            let dataLimpa = removeChars data_consult
            paciente <- not <$> ehPacienteValido (removeChars idPaciente)

            if not (ehValidoHorario horarioLimpo (horarios_atendimento medicoInfo)) then
                return "HORÁRIO INVÁLIDO OU NÃO É UM HORÁRIO DE ATENDIMENTO, HORÁRIOS NO PADRÃO HH:MM"
            else if not (ehDiaValido dataLimpa (dias_atendimento medicoInfo)) then
                return "DATA INVÁLIDA OU NÃO É UM DIA DE ATENDIMENTO, DATA NO PADRÃO AAAA-MM-DD"
            else if paciente then
                return "PACIENTE NÃO CADASTRADO NO SISTEMA"
            else do
                -- Lê o JSON de consultas e cria uma lista com todas
                consultasAntigas <- fromMaybe [] <$> readJsonFile pathConsultas
                let idConsulta = geraId dataLimpa horarioLimpo (removeChars medico)
                if ehIdUnico consultasAntigas idConsulta then do
                    let novaConsulta = Consulta idConsulta dataLimpa horarioLimpo (removeChars medico) (removeChars diagnostico) (removeChars idPaciente) "Em andamento"
                    writeJsonFile pathConsultas (consultasAntigas ++ [novaConsulta])
                    atualizaAtendimentos (removeChars medico)
                    insertAppointment (removeChars idPaciente) idConsulta
                    return "CONSULTA REGISTRADA"
                else
                    return "JÁ EXISTE UMA CONSULTA DESSE MÉDICO PARA ESSE DIA E HORÁRIO"
        Nothing -> return "MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO"


-- Adiciona uma consulta ao paciente
insertAppointment :: String -> String -> IO String
insertAppointment idPaciente consulta = do
    patients <- fromMaybe [] <$> readJsonFile "./Patients/Patients.JSON"
    case find (\p -> id_patient p == idPaciente) patients of
        Just patient -> do
            let updatedPatients = map (\p -> if id_patient p == idPaciente then patient {consultas = consultas patient ++ [consulta]} else p) patients
            writeJsonFile "./Patients/Patients.JSON" updatedPatients
            return "\n"
        Nothing -> return "Paciente não cadastrado no sistema"

-- Verifica se o novo Status é válido
ehStatusValido :: String -> Bool
ehStatusValido a = a `elem` ["Cancelada", "Concluída"]
-- Função para filtrar um elemento de uma lista dentro de uma tupla
filtrarElemento :: Eq b => b -> [(a, [b])] -> [(a, [b])]
filtrarElemento x = map (Data.Bifunctor.second (filter (/= x)))
-- Altera o status da Consulta
updateAppointment :: String -> String -> IO String
updateAppointment idConsulta novoStatus = do
    consultas <- fromMaybe [] <$> readJsonFile "./Appointments/Appointments.JSON"

    if not (ehStatusValido (removeChars novoStatus)) then
        return "NOVO STATUS INVÁLIDO"
    else
        case find (\c -> id_consulta c == removeChars idConsulta && status_consulta c == "Em andamento") consultas of
            Just consulta -> do
                let updatedAppoints = map (\c -> if id_consulta c == removeChars idConsulta then consulta {status_consulta = removeChars novoStatus} else c) consultas
                writeJsonFile "./Appointments/Appointments.JSON" updatedAppoints
                return "STATUS DA CONSULTA ATUALIZADO COM SUCESSO"
            Nothing -> return "ID DA CONSULTA INVÁLIDO/CONSULTA NÃO EXISTE OU CONSULTA JÁ FINALIZADA"

checkSchedule :: [String] -> IO String
checkSchedule [nomeMed] = do
    -- Remove caracteres indesejados da ID do médico
    let nomeMedLimpo = removeChars nomeMed

    -- Lê o conteúdo do arquivo JSON de usuários
    content <- fromMaybe [] <$> readJsonFile "./Users/Users.JSON"

    -- Filtra os usuários que correspondem ao ID e função de médico
    case find (\u -> funcao u == "MEDICO" && Users.User.nome u == nomeMedLimpo) content of
        Just fMedico -> do
            let hAtendimento = horarios_atendimento fMedico
                dAtendimento = dias_atendimento fMedico

            -- Lê o conteúdo do arquivo JSON de consultas
            contentApo <- fromMaybe [] <$> readJsonFile "./Appointments/Appointments.JSON"

            -- Filtra as consultas em andamento para o médico
            let consultasEmAndamento = filter (\f -> status_consulta f == "Em andamento" && medico_responsavel f == nomeMedLimpo) contentApo

            if not (null consultasEmAndamento)
                then do
                    let hConsulta = map (\c -> (diaDaSemana (data_consulta c), horario_consulta c)) consultasEmAndamento
                    return (strTransformer (subtrairListas (criaTuplas dAtendimento hAtendimento) hConsulta))
                else return (strTransformer (criaTuplas dAtendimento hAtendimento))

        Nothing -> return "x x x Não existe médico com essa ID. x x x \n"


-- Função generalizada para criar tuplas
criaTuplas :: [a] -> [b] -> [(a, [b])]
criaTuplas lista1 lista2 = [(elemento, lista2) | elemento <- lista1]

-- Função para subtrair os horários das tuplas correspondentes aos dias das consultas
subtrairListas :: (Eq a, Eq b) => [(a, [b])] -> [(a, b)] -> [(a, [b])]
subtrairListas original [] = original
subtrairListas original remover =
    foldr (\(diaRemover, horarioRemover) acc ->
        map (\(dia, horarios) ->
            if dia == diaRemover
            then (dia, filter (/= horarioRemover) horarios)
            else (dia, horarios)
        ) acc
    ) original remover

diaDaSemana :: String -> String
diaDaSemana dataStr =
    case parseDate dataStr of
        Just day -> case formatTime defaultTimeLocale "%A" day of
            "Monday"    -> "SEGUNDA"
            "Tuesday"   -> "TERCA"
            "Wednesday" -> "QUARTA"
            "Thursday"  -> "QUINTA"
            "Friday"    -> "SEXTA"
            "Saturday"  -> "SABADO"
            "Sunday"    -> "DOMINGO"
            _           -> "DIA DESCONHECIDO"
        Nothing -> "DATA INVÁLIDA"


strTransformer :: (Show a, Show b) => [(a, [b])] -> String
strTransformer [] = ""  -- Retorna uma string vazia se a lista estiver vazia
strTransformer tuplasList = concatMap (\(a, bs) -> concatenaTupla (a, bs) ++ "\n") tuplasList



concatenaTupla :: (Show a, Show b) => (a, [b]) -> String
concatenaTupla (a, bs) = show a ++ concatMap show bs ++ "\n"

-- Visualizar Consultas do Paciente
viewPatientAppointment :: [String] -> IO String
viewPatientAppointment idsConsultas = do

    consultas <- fromMaybe [] <$> readJsonFile "./Appointments/Appointments.JSON"

    -- Filtra as consultas correspondentes aos IDs fornecidos
    let consultasFiltradas = filter (\c -> id_consulta c `elem` idsConsultas) consultas

    -- Verifica se há consultas filtradas e retorna a mensagem apropriada
    if null consultasFiltradas
        then return "O Paciente não possui consultas."
        else return $ concatMap formatConsulta consultasFiltradas

-- Função para formatar uma consulta
formatConsulta :: Consulta -> String
formatConsulta c =
    "\nID da Consulta: " ++ id_consulta c ++
    "\nData: " ++ data_consulta c ++
    "\nHorário: " ++ horario_consulta c ++
    "\nMédico Responsável: " ++ medico_responsavel c ++
    "\nDiagnóstico: " ++ diagnostico c ++
    "\nStatus: " ++ status_consulta c ++ "\n"

