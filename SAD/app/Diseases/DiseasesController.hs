module Diseases.DiseasesController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)

data Diseases = Diseases {
    doenca                    :: String,
    especialidadeRelacionada  :: String,
    sintomasAssociados        :: [String],
    possivel_causa            :: String
} deriving (Show)

instance FromJSON Diseases where
    parseJSON = withObject "Diseases" $ \v -> Diseases
        <$> v .: fromString "doenca"
        <*> v .: fromString "especialidade_relacionada"
        <*> v .: fromString "sintomas_associados"
        <*> v .: fromString "possivel_causa"

viewDisease :: IO String
viewDisease = do
    content <- B.readFile "./Diseases/Diseases.JSON"
    let diseases = fromMaybe [] (decode content :: Maybe [Diseases])
        result = concatMap formatDiseases diseases
    return result
  where
    formatDiseases s = "Doença: " ++ doenca s ++ "\n" ++
                      "Possíveis Causas: " ++ possivel_causa s ++ "\n\n"
