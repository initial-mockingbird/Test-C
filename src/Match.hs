module Match where


import qualified AA as Map

import qualified Text.Read as Text
import Text.ParserCombinators.ReadPrec
import Data.Bifunctor 

------------------------------
-- Types                     |
------------------------------

newtype Target = T {getTarget :: String}
newtype Guess  = G {getGuess  :: String}

data Match 
    = Absent    Char 
    | Misplaced Char 
    | Correct   Char 

------------------------------
-- Instances                 |
------------------------------

instance Show Match where
    show (Absent c)    = ['\11035',c]
    show (Misplaced c) = ['\129000',c]
    show (Correct c)   = ['\129001',c]

instance Show Target where
    show T {getTarget=t} = "It was " ++ t

instance Show Guess where
    show G {getGuess=g}  = "Your guess: " ++ g

instance Read Match where
    readPrec     =  Text.parens $ do
        skipSpaces
        color <- get 
        skipSpaces
        c     <- get
        skipSpaces
        
        case color of
            '\11035'  -> return (Absent c)
            '\129000' -> return (Misplaced c)
            '\129001' -> return (Correct c)
            _         -> pfail
        where 
            skipSpaces :: ReadPrec ()
            skipSpaces = do
                l <- look
                let n = length $ takeWhile (`elem` [' ','\t']) l
                traverse_ (const get) [1..n]
            
            void = (pure () <*)
            traverse_ f = void . traverse f

------------------------------
-- Functions                 |
------------------------------


match :: Guess -> Target -> [Match]
match G {getGuess=g} T {getTarget=t}  = snd $ match' Map.empty g t
    where 
        match' :: Map.AA Char () -> String -> String -> (Map.AA Char (), [Match])
        match' _     [] [] = (Map.empty, [])
        match' preds (g:gs) (t:ts) 
            | g == t     = f (Correct g :) res 
            | Map.member g preds || Map.member g post = f (Misplaced g :)  res
            | otherwise  = f (Absent g :) res  
            where
                res@(post,_) = match' (Map.insert g () preds) gs ts 
                f = bimap (Map.insert t ())
        match' _ _ _ = undefined 

fullMatch :: [Match] -> Bool
fullMatch = all isCorrect
    where
        isCorrect (Correct _) = True 
        isCorrect _           = False

