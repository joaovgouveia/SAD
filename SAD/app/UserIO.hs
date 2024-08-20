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
    id                  :: String,
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


showLogin::IO()
showLogin = do
    putStrLn "> ID: "
    hFlush stdout
    idUser <- getLine    
    putStrLn "> SENHA: "
    hFlush stdout
    senhaUser <- getLine

    content <- B.readFile "./Users/Users.JSON"
    let id = decode content :: Maybe Usuario
        senha = decode content :: Maybe Usuario
        -- idFiltrados = filter (\login -> id login == idUser) id
    
    case id of
       Just u -> if idUser == id u 
                    then putStrLn "Tem id"
                    else putStrLn "Sem id"
       Nothing -> putStrLn "Falha ao decodificar o JSON."
    -- content <- B.readFile "./Users/Users.JSON"
    -- let id = fromMaybe [] (decode content :: Maybe [id])
    --     senha = fromMaybe [] (decode content :: Maybe [senha])
    --     idFiltrados = filter (\login -> id login == idUser) id
    --     senhaFiltrados = filter (\s -> senha s == senhaUser) senha
    -- if length idFiltrados == 1 && length senhaFiltrados == 1 then showStartMenu
    -- else "x x x Usuário Inválido! x x x\n Verifique seu ID e SENHA e tente novamente." showLogin 


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