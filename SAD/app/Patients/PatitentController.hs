module Patients.PatitentController where

import Data.Aeson (FromJSON, encode, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseJSON)
import Patients.Patient (Patient(..))

viewPatients::IO String
viewPatients = do
    content <- B.readFile "./Patients/Patients.JSON"
    let patients = fromMaybe [] (decode content :: Maybe [Patient])
        result = concatMap formatPatient patients
    return result
    where
        formatPatient p = "\nNome: " ++ nome p ++ "\nIdade: " ++ idade p ++ "\n"

createPatient::String -> String -> IO String
createPatient n age = do
    let patient = Patient {id = genreteID n, nome = n, idade = age, consultas = []}
    B.writeFile "myfile.json" (encode patient)

generateID::String -> String
generateID name = (take 1 name) ++ "1234"