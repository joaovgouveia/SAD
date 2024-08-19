module UserIO where

import Controller ( execute )
import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )


showStartMenu::IO()
showStartMenu = do
    putStrLn "=====================================================\nBem vindo ao SAD (Sistema Automático de diagnósticos)\n=====================================================\ndigite 'help' para abrir a lista de funções do programa.\n"
    showMenu

-- Função principal do menu inicial
showMenu :: IO ()
showMenu = do
    putStrLn "> Opção: "
    hFlush stdout
    line <- getLine
    if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("Resposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenu

-- Função auxiliar para dividir a linha de entrada em comandos e argumentos
splitLine :: Char -> String -> [String]
splitLine _ "" = []
splitLine delimiter str = 
    let (word, rest) = break (== delimiter) str
    in word : case rest of
                [] -> []
                (_:rest') -> splitLine delimiter rest'

exit::IO()
exit = exitSuccess