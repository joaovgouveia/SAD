module UserIO where
import Data.Aeson (FromJSON, decode, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Controller ( execute )
import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types (parseJSON)
import Data.Maybe (fromMaybe)

data Usuario = Usuario {
    usuarioId           :: String,
    funcao              :: String,
    especialidade       :: String,
    dias_atendimento    :: [String],
    horarios_atendimento :: [String],
    nome                :: String,
    pacientes_atendidos :: String,
    senha               :: String
} deriving (Show)

instance FromJSON Usuario where
    parseJSON = withObject "Usuario" $ \v -> Usuario
        <$> v .: fromString "id"
        <*> v .: fromString "funcao"
        <*> v .: fromString "especialidade"
        <*> v .: fromString "dias_atendimento"
        <*> v .: fromString "horarios_atendimento"
        <*> v .: fromString "nome"
        <*> v .: fromString "pacientes_atendidos"
        <*> v .: fromString "senha"


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
            let mUsuarios = decode content :: Maybe [Usuario]
        
            case mUsuarios of
                Just usuarios -> 
                    -- Filtra os usuários que correspondem ao ID e senha fornecidos
                    let fUsuarios = filter (\u -> usuarioId u == idUser && senha u == senhaUser) usuarios
                    in if null fUsuarios
                        then do
                            putStrLn "x x x ID ou senha incorretos. x x x \n"
                            showLogin
                        else do
                            let usuarioLogado = head fUsuarios
                            putStrLn "Login bem-sucedido!\n"
                            showStartMenu
                            case funcao usuarioLogado of
                                "MEDICO" -> showMenuMedico
                                "ADMINISTRADOR" -> showMenuAdm
                                "SECRETARIA" -> showMenuSec
                                _ -> putStrLn "Função desconhecida."
                Nothing -> putStrLn "Falha ao decodificar o JSON."


showStartMenu::IO()
showStartMenu = do
    putStrLn "=====================================================\nBem vindo ao SAD (Sistema Automático de diagnósticos)\n=====================================================\ndigite 'help' para abrir a lista de funções do programa.\n"
    --showMenu

-- Função principal do menu inicial
-- showMenu :: IO ()
-- showMenu = do
--     putStrLn "> Opção: "
--     hFlush stdout
--     line <- getLine
--     if line == "exit" then exit
--     else if null line then putStrLn "Nenhum comando foi digitado."
--     else do 
--         resposta <- execute (splitLine ' ' line)
--         putStrLn ("Resposta:\n" ++ resposta)

--     -- Chama a função novamente para o próximo comando
--     showMenu

showMenuMedico :: IO ()
showMenuMedico = do
    putStrLn "> Médico, escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./funcoesHELP.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("Resposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenuMedico


showMenuAdm :: IO ()
showMenuAdm = do
    putStrLn "> Administrador, escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./funcoesHELP.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("Resposta:\n" ++ resposta)

    -- Chama a função novamente para o próximo comando
    showMenuAdm


showMenuSec :: IO ()
showMenuSec = do
    putStrLn "> Secretário, escolha a opção: "
    hFlush stdout
    line <- getLine
    if line == "help" then do
        help <- readFile "./funcoesHELP.txt"
        putStrLn ("\n" ++ help)
    else if line == "exit" then exit
    else if null line then putStrLn "Nenhum comando foi digitado."
    else do 
        resposta <- execute (splitLine ' ' line)
        putStrLn ("Resposta:\n" ++ resposta)

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