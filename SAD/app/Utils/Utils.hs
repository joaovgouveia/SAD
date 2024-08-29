module Utils.Utils where

import Data.Aeson (ToJSON, FromJSON, decode, encode)
import qualified Data.ByteString.Lazy as B

-- Lê de um JSON
readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe [a])
readJsonFile path = do
    content <- B.readFile path
    return (decode content)

-- Escreve em um JSON
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path = B.writeFile path . encode

-- Remove caracteres potÊncialmente indesejados
removeChars :: String -> String
removeChars = filter (`notElem` "[]\",")

-- Cria uma lista que representa a interseção entre A e B
intersectionList :: Eq a => [a] -> [a] -> [a]
intersectionList x y = [z | z <- x, z `elem` y]

-- Retorna True caso a primeira lista pertença a segunda
belongs :: Eq a => [a] -> [a] -> Bool
belongs (h:t) l
    | null t && h `elem` l = True
    | h `elem` l = belongs t l
    | otherwise = False

-- Retorna True caso a primeira lista intersecte a segunda
intersect :: Eq a => [a] -> [a] -> Bool
intersect x y
    | null (intersectionList x y) = False
    | otherwise = True

-- Remove duplicatas de listas
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)