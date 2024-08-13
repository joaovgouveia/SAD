-- Interface do usuário, pega entrada e mostra saída. Se comunica com o controller
module UserIO where
import Controller

showStartMenu::IO()
showStartMenu = do
    putStrLn "place_holder"
    showMenu

showMenu::IO()
showMenu = do
    command <- getLine
    putStrLn ("> " ++ execute command)
    showMenu
    
execute::String -> String
execute c
    | c == "duck" = duck
    | otherwise = "Funcao nao existe"