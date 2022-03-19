module Play where 
-- Aca falta un export de initialState al programa principal

import Util
import System.Random (randomRIO)
import Match -- No deberian ser necesarios 
import AA    
import System.IO 
import Control.Monad
import Data.Char
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

instance Show GameState where
    show GS {played=_played, won=_won, streak=_streak} 
        =  "Played: "  ++ show _played 
        ++ " Won: "    ++ show _won 
        ++ " Lost: "   ++ show (_played - _won) 
        ++ " Streak: " ++ show _streak

instance Show Result where
    show (Win (T t))  = "Got it! It was «" ++ t ++ "» 😎 "
    show (Lose (T t)) = "Bummer! It was «" ++ t ++ "» 💀 "

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

playTheGame :: GameState -> IO ()
playTheGame state = do
    newTarget <- pickTarget (dict state)
    res <- play (changeTarget state newTarget) 
    let newState = updState state res

    print state
    print newTarget
    continue <- yesOrNo "Desea continuar"

    if continue 
        then playTheGame newState 
        else putStrLn "End of the game"
    
    where
        updState :: GameState -> Result -> GameState
        updState (GS p w s t d) (Win _) = GS (p+1) (w+1) s (T "") d
        updState (GS p w s t d) (Lose _) = GS (p+1) w (s+1) (T "") d
        changeTarget :: GameState -> Target -> GameState
        changeTarget (GS p w s t d) t2 = GS p w s t2 d

-- PARA PROBAR TEMPORALMENTE, LUEGO BORRAR
menu = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    state <- initialState
    playTheGame state


play :: GameState -> IO Result
play state = play' state 1 turns

play' :: GameState -> Int -> Int -> IO Result
play' gs current max =  do
    putStr $ "Guess " ++ show current ++ "? "
    word <- readFive 

    let t = target gs
        d = dict gs 
        m = match (G word) t
        printMatch = (putStr . show)  m >> putChar '\n'

    case (word `AA.member` d, fullMatch m) of
        (False,_) -> putStrLn ("Your guess: '" ++ word ++ "' is not a valid word!") >> play' gs current max
        (_,True)  -> printMatch >> pure (Win t)
        (_,False) -> printMatch >> if current + 1 == max then pure $ Lose t else play' gs (current +1) max


readFive :: IO String
readFive =  reverse <$> readFive' 5 5 ""

readFive' :: Int -> Int -> String -> IO String
readFive' n max acc = do
    c <- toLower <$>  getChar'

    case (c, c `elem` ['a'..'z']) of 
        ('\b',_) -> when (n < max) eraseChar >> readFive' (min max (n+1)) max (drop 1 acc)
        ('\n',_) -> if n == 0 then putChar '\n' >> pure acc else readFive'  n max acc 
        ('\r',_) -> if n == 0 then putChar '\n' >> pure acc else readFive'  n max acc 
        (_,True) -> if n > 0 then putChar c >> readFive' (n-1) max (c:acc) else readFive' n max acc 
        _        -> readFive' n max acc 


eraseChar :: IO ()
eraseChar = putChar '\b' >> putChar ' ' >> putChar '\b'

eraseNChar :: Int -> IO ()
eraseNChar = flip replicateM_ (eraseChar >> putChar ' ') >=> const (putChar '\b')

pickTarget :: AA String String -> IO Target
pickTarget arbol = do
    str <- ( lista !!) <$> randomRIO (0,l-1)
    return $ T str
    where (lista,l) = treeToList arbol 

treeToList :: AA String String -> ( [String], Int ) 
treeToList arbolAA = foldr unir ([],0) arbolAA 
    where unir str (lista,l) = (lista ++ [str],l+1) 
