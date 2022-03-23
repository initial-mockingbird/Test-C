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
