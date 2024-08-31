module Users.UserController where

import Data.Maybe (fromMaybe)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Users.User (User(..))
import Utils.Utils (readJsonFile)

pathUsers :: String
pathUsers = "./Users/Users.JSON"

viewUsers :: IO String
viewUsers = do
    users <- fromMaybe [] <$> readJsonFile pathUsers
    return (unwords (map formatUser users))

viewUser :: String -> IO String
viewUser i = do
    users <- fromMaybe [] <$> readJsonFile pathUsers
    let found = filter (\u -> Users.User.id u == i) users
    if null found then return "Usuário não encontrado."
    else return (formatUser (head found))

viewUsersByFunction :: String -> IO String
viewUsersByFunction func = do
    users <- fromMaybe [] <$> readJsonFile pathUsers
    let found = filter (\u -> funcao u == func) users
    if null users then return "Usuário não encontrado."
    else return (unwords (map formatUser found))

viewMedicos :: IO String
viewMedicos = do
    users <- fromMaybe [] <$> readJsonFile pathUsers
    let medicosFiltrados = filter (\m -> funcao m == "MEDICO") users
        result = concatMap formatMedico medicosFiltrados
    return result
    where
      formatMedico m = "Nome: " ++ nome m ++ "\n" ++
                       "Especialidade: " ++ especialidade m ++ "\n" ++
                       "Dias de Atendimento: " ++ unwords (dias_atendimento m) ++ "\n" ++
                       "Horários de Atendimento: " ++ unwords (horarios_atendimento m) ++ "\n\n"

viewAtuation :: IO String
viewAtuation = do
    users <- fromMaybe [] <$> readJsonFile pathUsers
    let medicosFiltrados = filter (\m -> funcao m == "MEDICO") users
        agrupadosPorEspecialidade = groupBy ((==) `on` especialidade) $ sortOn especialidade medicosFiltrados
        resultado = concatMap formatEspecialidade agrupadosPorEspecialidade
    return resultado
  where
    formatEspecialidade :: [User] -> String
    formatEspecialidade [] = ""
    formatEspecialidade (m:ms) = especialidade m ++ ":\n" ++ 
                                 unlines (map nome (m:ms)) ++ "\n"

formatUser :: User -> String
formatUser u = "Nome: " ++ nome u ++
               "\nFunção: " ++ funcao u ++ "\n\n"