module UserIO where

import Controller (execute)
import System.IO (hFlush, stdout)

-- Função principal do menu inicial
showStartMenu :: IO ()
showStartMenu = do
    putStrLn "> Opção: "
    hFlush stdout
    line <- getLine
    if null line
        then putStrLn "Nenhum comando foi digitado."
        else do
            resposta <- execute (splitLine ' ' line)
            putStrLn ("Resposta:\n" ++ resposta)
    -- Chama a função novamente para o próximo comando
    showStartMenu

-- Função auxiliar para dividir a linha de entrada em comandos e argumentos
splitLine :: Char -> String -> [String]
splitLine _ "" = []
splitLine delimiter str = 
    let (word, rest) = break (== delimiter) str
    in word : case rest of
                [] -> []
                (_:rest') -> splitLine delimiter rest'
