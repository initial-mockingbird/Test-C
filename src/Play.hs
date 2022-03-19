module Play where 
-- Aca falta un export de initialState al programa principal

import Util
import System.Random (randomRIO)
import Match -- No deberian ser necesarios 
import AA    

---------------------------------
-- Types                        |
---------------------------------

data GameState = GS { 
    played :: Int, 
    won :: Int,
    streak :: Int, 
    target :: Target, 
    dict :: AA.AA String String
}

data Result = Win Target | Lose Target

---------------------------------
-- Instances                    |
---------------------------------

-- DANI ACAAAAAAAAAAAAAAAAAAAAAA
instance Show GameState where
    show state = "Estado actual"

-- DANI ACAAAAAAAAAAAAAAAAAAAAAA
instance Show Result where
    show state = "Todos somos ganadores en el juego de la vida"

---------------------------------
-- Functions Auxiliares         |
---------------------------------

takeAA :: GameState ->  AA String String
takeAA (GS p w s t d) = d 

---------------------------------
-- Functions                    |
---------------------------------

initialState :: IO GameState 
initialState = do
    arbol <- loadDictionary dictionary
    return $ GS 0 0 0 (T "") arbol

playTheGame state = do
    newTarget <- pickTarget (dict state)
    res <- play (changeTarget state newTarget) 
    let newState = updState state res

    print state
    print newTarget
    continue <- yesOrNo "Desea continuar"

    if continue then playTheGame newState 
    else return "End of the game"
    
    where
        updState :: GameState -> Result -> GameState
        updState (GS p w s t d) (Win _) = GS (p+1) (w+1) s (T "") d
        updState (GS p w s t d) (Lose _) = GS (p+1) w (s+1) (T "") d
        changeTarget :: GameState -> Target -> GameState
        changeTarget (GS p w s t d) t2 = GS p w s t2 d

-- PARA PROBAR TEMPORALMENTE, LUEGO BORRAR
menu = do
    state <- initialState
    playTheGame state

-- DANI ACAAAAAAAAAAAAAAAAAAAAAA
play :: GameState -> IO Result
play state = return $ Win $ T "Ganador"

pickTarget :: AA String String -> IO Target
pickTarget arbol = do
    str <- ( lista !!) <$> randomRIO (0,l-1)
    return $ T str
    where (lista,l) = treeToList arbol 

treeToList :: AA String String -> ( [String], Int ) 
treeToList arbolAA = foldr unir ([],0) arbolAA 
    where unir str (lista,l) = (lista ++ [str],l+1) 
