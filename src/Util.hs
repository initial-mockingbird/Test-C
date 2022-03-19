module Util where 

import System.IO  
import Control.Monad
import Prelude hiding (lookup)
import Match
import AA

-- Number of turns in the game
turns :: Int
turns = 6

-- Path of the file where are saved the words
dictionary :: String
dictionary = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/american-english"

---------------------------------
-- Functions                    |
---------------------------------

validWord :: String -> Bool
validWord str 
    | (length str==5) && ( foldr fAnd True $ map isLetter str ) = True
    | otherwise = False
    where
        fAnd :: Bool -> Bool -> Bool
        fAnd x y = x && y
        isLetter :: Char -> Bool
        isLetter c = ('a' <= c && c <= 'z')

fiveLetterWords :: [String] -> [String]
fiveLetterWords ls = filter validWord ls

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let lista = fiveLetterWords $ words contents
    return $ foldr unir empty lista
    where  
        unir :: String -> AA String String -> AA String String
        unir str arbol = insert str str arbol

yesOrNo :: String -> IO Bool
yesOrNo text = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    putStr ( text ++ " (y/n) ?" )
    c <- getChar
    if c=='y' then putStr " y\n" >>return True
    else if c=='n' then putStr " n\n" >>return False
    else putStr "\n" >> yesOrNo text

