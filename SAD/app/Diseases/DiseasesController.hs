module Diseases.DiseasesController where

import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
import Diseases.Disease(Disease(..))

viewDisease :: IO String
viewDisease = do
    content <- B.readFile "./Diseases/Diseases.JSON"
    let diseases = fromMaybe [] (decode content :: Maybe [Disease])
        result = concatMap formatDisease diseases
    return result

formatDisease::Disease -> String
formatDisease s = "Doença: " ++ doenca s ++ "\n" ++
                  "Possíveis Causas: " ++ possivelCausa s ++
                  "Medicações indicadas: " ++ unwords (medicamentos s) ++ "\n\n"