{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Diagnosis.Diagnosis where

import Data.Maybe (fromMaybe)
import Data.Function ( on )
import Utils.Utils (readJsonFile, belongs, intersect, intersectionList, maxValue, removeDuplicates, mostCommonElem)
import Diseases.Disease (Disease(..))
import Users.User(User(..))
import Diseases.DiseasesController (formatDisease)

findDisease :: [String] -> IO String
findDisease symptons = do
    let pathDiseases = "./Diseases/Diseases.JSON"
    diseases <- fromMaybe [] <$> readJsonFile pathDiseases

    let filterDiseases = filter (intersect symptons . sintomas_associados) diseases
    let maxPercent = maxValue (map calculateProp filterDiseases)
    let mostLikely = filter (\x -> calculateProp x == maxPercent) filterDiseases
    let chosenDoctor = mostCommonElem (map especialidade_relacionada mostLikely)
    let response = "\nDiagnóstico:\n" ++ unwords (map formatDisease mostLikely) ++ "Certeza: " ++ show maxPercent ++ "%\nTipo de Médico Indicado: " ++ chosenDoctor ++"\n"

    if null filterDiseases then return "Diagnóstico incerto"
    else return response

    where calculateProp x = round (factor_a x * 100)
          factor_a x = divide (length (intersecSD x)) (length (sintomas_associados x))
          intersecSD x = intersectionList symptons (sintomas_associados x)
          divide = (/) `on` fromIntegral