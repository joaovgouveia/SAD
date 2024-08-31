module Appointments.AppointmentController where

import Data.Aeson (decode, encode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as B
import Appointments.Appointment
import Data.Maybe (fromMaybe)
import Data.List (find, any)
import Data.Time (parseTimeM, defaultTimeLocale, Day, formatTime)
import Text.Read (readMaybe)
import Users.User (User (..))
import Patients.Patient (Patient (..))

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

-- Verifica se o paciente existe no sistema
ehPacienteValido :: String -> IO Bool
ehPacienteValido idPaciente = do
    pacientes <- fromMaybe [] <$> readJsonFile "./Patients/Patients.JSON"
    return $ any (\p -> id_patient p == idPaciente) pacientes

-- Função para ler JSON de um arquivo
readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)

-- Função para escrever JSON em um arquivo
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

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


-- Visualizar Consultas do Paciente
viewPatientAppointment :: [String] -> IO String
viewPatientAppointment idsConsultas = do
    -- Lê o arquivo JSON de consultas
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