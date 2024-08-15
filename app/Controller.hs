-- Controller geral do sistema, age como ponte entre o user_io e as outras partes do sistema
module Controller where

execute::[String] -> String
execute (cmd:args)
    | cmd == "addUser" = addUser args
    | cmd == "viewUser" = viewUser args
    | cmd == "deleteUser" = deleteUser args
    | cmd == "listUsers" = listUsers args
    | cmd == "addMedication" = addMedication args
    | cmd == "viewMedication" = viewMedication args
    | cmd == "listMedications" = listMedications args
    | cmd == "addDisease" = addDisease args
    | cmd == "viewDisease" = viewDisease args
    | cmd == "listDiseases" = listDiseases args
    | cmd == "addSymptom" = addSymptom args
    | cmd == "viewSymptom" = viewSymptom args
    | cmd == "listSymptoms" = listSymptoms args
    | otherwise = "Função nao existe"

-- Funções do sistema
addUser::[String] -> String
addUser args = ""

viewUser::[String] -> String
viewUser args = ""

deleteUser::[String] -> String
deleteUser args = ""

listUsers::[String] -> String
listUsers args = ""

addMedication::[String] -> String
addMedication args = ""

viewMedication::[String] -> String
viewMedication args = ""

listMedications::[String] -> String
listMedications args = ""

addDisease::[String] -> String
addDisease args = ""

viewDisease::[String] -> String
viewDisease args = ""

listDiseases::[String] -> String
listDiseases args = ""

addSymptom::[String] -> String
addSymptom args = ""

viewSymptom::[String] -> String
viewSymptom args = ""

listSymptoms::[String] -> String
listSymptoms args = ""

-- TODO: Consultas
duck::String
duck = "quack"
