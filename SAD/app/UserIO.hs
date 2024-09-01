module UserIO where
import Data.Aeson (decode)
import Controller ( execute )
import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )
import qualified Data.ByteString.Lazy as B
import Users.User(User(..))

showLogin :: IO ()
showLogin = do
    putStrLn "[ID]: "
    hFlush stdout
    idUser <- getLine   
    if idUser == "exit" then exit
    else do 
        putStrLn "[SENHA]: "
        hFlush stdout
        senhaUser <- getLine
        if senhaUser == "exit" then exit
        else do 
            -- Lê o conteúdo do arquivo JSON
            content <- B.readFile "./Users/Users.JSON"
    
            -- Decodifica o JSON para uma lista de usuários
            let mUsers = decode content :: Maybe [User]
        
            case mUsers of
                Just users -> 
                    -- Filtra os usuários que correspondem ao ID e senha fornecidos
                    let fUsers = filter (\u -> Users.User.id u == idUser && senha u == senhaUser) users
                    in if null fUsers
                        then do
                            putStrLn "x x x ID ou senha incorretos. x x x \n"
                            showLogin
                        else do
                            let userLogado = head fUsers
                            putStrLn "Login bem-sucedido!\n"
                            case funcao userLogado of
                                "MEDICO" -> showStartMenuMedico
                                "ADMINISTRADOR" -> showStartMenuAdm
                                "SECRETARIA" -> showStartMenuSec
                                _ -> putStrLn "Função desconhecida."
                Nothing -> putStrLn "Falha ao decodificar o JSON."


showStartMenuMedico::IO()
showStartMenuMedico = do
    sadMenu <- readFile "./startMenuMedico.txt"
    putStrLn sadMenu
    showMenuMedico


showStartMenuAdm::IO()
showStartMenuAdm = do
    sadMenu <- readFile "./startMenuAdm.txt"
    putStrLn sadMenu
    showMenuAdm

showStartMenuSec::IO()
showStartMenuSec = do
    sadMenu <- readFile "./startMenuSec.txt"
    putStrLn sadMenu
    showMenuSec


showMenuMedico :: IO ()
showMenuMedico = do
    putStrLn "> Escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./helpMed.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("\nResposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenuMedico


showMenuAdm :: IO ()
showMenuAdm = do
    putStrLn "> Escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./helpAdm.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("\nResposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenuAdm


showMenuSec :: IO ()
showMenuSec = do
    putStrLn "> Escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./helpSec.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("\nResposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenuSec

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