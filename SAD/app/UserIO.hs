module UserIO where
import Controller
import System.Exit ( exitSuccess )

showStartMenu::IO()
showStartMenu = do
    putStrLn "=====================================================\nBem vindo ao SAD (Sistema Automático de diagnósticos)\n=====================================================\ndigite 'help' para abrir a lista de funções do programa."
    showMenu

showMenu::IO()
showMenu = do
    putStrLn "----------------------------------\n> Opção:"
    line <- getLine
    if line == "exit" then exit
    else putStrLn ("Resposta:\n" ++ execute (splitLine ' ' line))
    showMenu

splitLine :: Eq a => a -> [a] -> [[a]]
splitLine x y = func x y [[]]
    where
        func x [] z = reverse $ map reverse z
        func x (y:ys) (z:zs) = if y==x then 
            func x ys ([]:(z:zs)) 
        else 
            func x ys ((y:z):zs)

exit::IO()
exit = exitSuccess