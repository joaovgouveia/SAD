module Users.UserController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)

main :: IO ()
main = do
    result <- viewMedicos
    putStrLn result

data Medico = Medico {
    nome                :: String,
    especialidade       :: String,
    horariosAtendimento :: [String],
    dias_atendimento :: [String]
} deriving (Show)

instance FromJSON Medico where
    parseJSON = withObject "Medico" $ \v -> Medico
        <$> v .: fromString "nome"
        <*> v .: fromString "especialidade"
        <*> v .: fromString "horarios_atendimento"
        <*> v .: fromString "dias_atendimento"

viewMedicos :: IO String
viewMedicos = do
    content <- B.readFile "./Users/Medicos.JSON"
    let medicos = fromMaybe [] (decode content :: Maybe [Medico])
        result = concatMap formatMedico medicos
    return result
  where
    formatMedico m = "Nome: " ++ nome m ++ "\n" ++
                     "Especialidade: " ++ especialidade m ++ "\n" ++
                     "Horários de Atendimento: " ++ unwords (horariosAtendimento m) ++ "\n" ++
                     "Dias de Atendimento: " ++ unwords (dias_atendimento m) ++ "\n\n"