module Symptons.SymptomController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)

data Symptom = Symptom {
    doenca                    :: String,
    especialidadeRelacionada  :: String,
    sintomasAssociados        :: [String]
} deriving (Show)

instance FromJSON Symptom where
    parseJSON = withObject "Symptom" $ \v -> Symptom
        <$> v .: fromString "doenca"
        <*> v .: fromString "especialidade_relacionada"
        <*> v .: fromString "sintomas_associados"

viewSymptom :: IO String
viewSymptom = do
    content <- B.readFile "./Diseases/Diseases.JSON"
    let symptoms = fromMaybe [] (decode content :: Maybe [Symptom])
        result = concatMap formatSymptom symptoms
    return result
  where
    formatSymptom s = "Doença: " ++ doenca s ++ "\n" ++
                      "Sintomas Associados: " ++ unwords (sintomasAssociados s) ++ "\n\n"
