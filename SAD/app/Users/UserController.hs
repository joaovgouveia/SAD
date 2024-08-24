module Users.UserController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Users.User (User(..))

viewMedicos :: IO String
viewMedicos = do
    content <- B.readFile "./Users/Users.JSON"
    let medicos = fromMaybe [] (decode content :: Maybe [User])
        medicosFiltrados = filter (\m -> funcao m == "MEDICO") medicos
        result = concatMap formatMedico medicosFiltrados
    return result
  where
    formatMedico m = "Nome: " ++ nome m ++ "\n" ++
                     "Especialidade: " ++ especialidade m ++ "\n" ++
                     "Dias de Atendimento: " ++ unwords (dias_atendimento m) ++ "\n" ++
                     "Horários de Atendimento: " ++ unwords (horarios_atendimento m) ++ "\n\n"

viewAtuation :: IO String
viewAtuation = do
    content <- B.readFile "./Users/Users.JSON"
    let medicos = fromMaybe [] (decode content :: Maybe [User])
        medicosFiltrados = filter (\m -> funcao m == "MEDICO") medicos
        agrupadosPorEspecialidade = groupBy ((==) `on` especialidade) $ sortOn especialidade medicosFiltrados
        resultado = concatMap formatEspecialidade agrupadosPorEspecialidade
    return resultado
  where
    formatEspecialidade :: [User] -> String
    formatEspecialidade [] = ""
    formatEspecialidade (m:ms) = especialidade m ++ ":\n" ++ 
                                 unlines (map nome (m:ms)) ++ "\n"