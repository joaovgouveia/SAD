module Users.UserController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
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