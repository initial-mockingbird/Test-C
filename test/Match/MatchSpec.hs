{-# LANGUAGE ScopedTypeVariables #-}
module Match.MatchSpec where 

import Match (Match(..))
import qualified Match as M
import Text.Read
import AA (AA(..))
import qualified AA
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad
import Prelude hiding (lookup)
import Data.Foldable (traverse_)


newtype RandomMatch   = RM  {getRM :: Match} deriving Show

instance Arbitrary RandomMatch where
    arbitrary = do
        c <- arbitrary `suchThat` (`elem` (['a'..'z'] ++ ['A'..'Z']))
        let options = ($ c) <$> [Absent , Misplaced, Correct]
        RM <$> elements options

showSpec :: Spec
showSpec = describe "show spec" $ do
    prop "Show instance has exactly 2 characters" $ 
        \(RM m :: RandomMatch) -> case  show m  of
            [_,_] -> True
            _     -> False 

readSpec :: Spec
readSpec = describe "read Spec" $ do
    prop "Showing and then reading a match yields the identity" $ 
        \(RM m :: RandomMatch) -> case (readMaybe . show) m :: Maybe Match of
            Just _ -> True
            _      -> False 
    prop "trailing spaces don't matter" $
        \(RM m :: RandomMatch) (n :: Int) -> case (readMaybe . (++ replicate n ' ') . show) m :: Maybe Match of
            Just _ -> True
            _      -> False 
    prop "leading spaces don't matter" $
        \(RM m :: RandomMatch) (n :: Int) -> case (readMaybe . (replicate n ' ' ++) . show) m :: Maybe Match of
            Just _ -> True
            _      -> False 
    prop "middle spaces don't matter" $
        \(RM m :: RandomMatch) (n :: Int) -> case (readMaybe . (\[color,c] -> color : replicate n ' ' ++ [c]) . show) m :: Maybe Match of
            Just _ -> True
            _      -> False 

internalsSpecMatch :: IO ()
internalsSpecMatch = traverse_ hspec [showSpec,readSpec]