module Solve where

import AA (AA)
import qualified AA 
import Util 
import Match 
import Data.Functor 
import Data.Bifunctor
import Control.Monad
import System.Random
---------------------------------
-- Types                        |
---------------------------------

data Solver = Naive | Clever

data SolverState = GS 
    { suggestion :: String
    , possible :: [String]
    , remaining :: Int
    , dict :: AA.AA String String
    , strategy :: Solver
    }

---------------------------------
-- Instances                    |
---------------------------------

instance Show SolverState where
    show GS {suggestion=_suggestion,remaining=1}
        = "It must be <<" ++  _suggestion ++ ">>."
    show GS {suggestion=_suggestion,remaining=_remaining}
        = show _remaining ++ " words remain. I suggest: <<" ++  _suggestion ++ ">>."

---------------------------------
-- Functions                    |
---------------------------------

infixr 9 |>
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

initialSolver :: Solver -> IO SolverState
initialSolver strat 
    = loadDictionary dictionary 
    <&> diag 
    |> \ (r,d) -> GS 
        { suggestion=mempty
        , possible=[]
        , remaining=r
        , dict=d
        , strategy=strat}
    where
        diag :: AA a b -> (Int, AA a b)
        diag d = (length d,d)

        

solveTheGame :: SolverState -> IO ()
solveTheGame _ = pure ()

sieve :: [Match] -> [String] -> [String]
sieve match words = [word | Just word <- refine <$> words] 
    where 
        throwError :: Maybe a
        throwError = Nothing 

        decider :: Match -> Char -> Bool
        decider (Absent c)    c' = c==c'
        decider (Misplaced c) c' = c==c'
        decider (Correct c)   c' = c/=c'

        refine' :: AA Char () -> [Match] -> String -> Maybe (AA Char (),AA Char ())
        refine' blacksSoFar [] [] = pure (blacksSoFar, AA.empty)
        refine' blacksSoFar (m:ms) (t:ts) = do 

            let blacks'   = case m of Absent c -> AA.insert c () blacksSoFar; _ -> blacksSoFar
                generated = refine' blacks' ms ts

            res@(blacks,yellows) <- case m of 
                Misplaced c -> fmap (AA.insert c ()) <$> generated
                _           -> generated

            when (AA.member t blacks || decider m t) throwError
            
            pure res

        refine' _ _ _ = undefined 

        refine :: String -> Maybe String
        refine s 
            = refine' AA.empty match s >>= \(_,y) -> unless (hasYellow y s) throwError >> pure s

        hasYellow :: AA Char () -> String -> Bool 
        hasYellow = (.) null . foldr AA.delete

naive :: [Match] -> SolverState -> IO SolverState
naive  =  (.) setRandomWord . filterBad 
    
clever ::  [Match] -> SolverState -> SolverState
clever = (.) setBestWord . filterBad 

setBestWord :: SolverState -> SolverState
setBestWord s@GS {possible=_possible} = s{suggestion = bestWord _possible}
    where
        bestWord :: [String] -> String
        bestWord  = snd . minimum . fmap coupleScore

        coupleScore s = (length $ sieve (Correct <$> s) _possible ,s)



filterBad :: [Match] -> SolverState -> SolverState 
filterBad m s@GS{possible=_possible} = s{possible=p,remaining=n}
    where 
        p = sieve m _possible
        n = length p

getRandom :: [a] -> IO a
getRandom xs = (xs !!) <$> randomRIO (0, length xs - 1) 

setRandomWord :: SolverState -> IO SolverState
setRandomWord s@GS{possible=_possible}= getRandom _possible <&> \sugg -> s{suggestion=sugg}