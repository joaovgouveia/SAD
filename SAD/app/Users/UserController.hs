module Users.UserController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)

data Medico = Medico {
    id                  :: String,
    funcao              :: String,
    especialidade       :: String,
    dias_atendimento    :: [String],
    horarios_atendimento :: [String],
    nome                :: String,
    pacientes_atendidos :: String,
    senha               :: String
} deriving (Show)

instance FromJSON Medico where
    parseJSON = withObject "Medico" $ \v -> Medico
        <$> v .: fromString "id"
        <*> v .: fromString "funcao"
        <*> v .: fromString "especialidade"
        <*> v .: fromString "dias_atendimento"
        <*> v .: fromString "horarios_atendimento"
        <*> v .: fromString "nome"
        <*> v .: fromString "pacientes_atendidos"
        <*> v .: fromString "senha"

viewMedicos :: IO String
viewMedicos = do
    content <- B.readFile "./Users/Users.JSON"
    let medicos = fromMaybe [] (decode content :: Maybe [Medico])
        medicosFiltrados = filter (\m -> funcao m == "MEDICO") medicos
        result = concatMap formatMedico medicosFiltrados
    return result
  where
    formatMedico m = "Nome: " ++ nome m ++ "\n" ++
                     "Especialidade: " ++ especialidade m ++ "\n" ++
                     "Dias de Atendimento: " ++ unwords (dias_atendimento m) ++ "\n" ++
                     "Horários de Atendimento: " ++ unwords (horarios_atendimento m) ++ "\n\n"