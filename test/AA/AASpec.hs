{-# LANGUAGE ScopedTypeVariables #-}
module AA.AASpec where

import AA (AA(..))
import qualified AA
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad
import Prelude hiding (lookup)
import Data.Foldable (traverse_)

newtype RandomAA k v= RAA {getAA :: AA k v} deriving Show
newtype Different k v = Diff{getDiff :: (AA k v,k)} deriving Show
newtype SuperRandomAA k v = SRAA {getSRAA :: AA k v} deriving Show

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (RandomAA k v) where
    arbitrary = RAA . AA.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Different k v) where
    arbitrary = do
        xs <- listOf ((,) <$> arbitrary <*> arbitrary)
        x  <- suchThat arbitrary (`notElem` map fst xs)
        return $ Diff (AA.fromList xs,x)

instance (Arbitrary k, Arbitrary v) => Arbitrary (SuperRandomAA k v) where
    arbitrary = SRAA <$> (listOf ((,) <$> arbitrary <*> arbitrary) >>= fromList)
        where 
            fromList :: [(k,v)] -> Gen (AA k v)
            fromList = foldM insertR Empty 

            insertR :: AA k a -> (k,a) -> Gen (AA k a)
            insertR Empty (k,v) = pure Node {lvl=1, key=k, val=v, lAA=Empty, rAA=Empty}
            insertR (Node n k' v' l r) (k,v) = do
                choice <- elements [AA.L, AA.R]
                let node = Node (n+1) k' v' 
                case (choice,l,r) of
                    (AA.L, Empty,_) -> pure $ node (Node {lvl=1, key=k, val=v, lAA=Empty, rAA=Empty}) r 
                    (AA.R, _,Empty) -> pure $ node l (Node {lvl=1, key=k, val=v, lAA=Empty, rAA=Empty})
                    (AA.L,_,_) -> insertR l (k,v) >>= \l -> pure (node l r)
                    (AA.R,_,_) -> insertR r (k,v) >>= \r -> pure (node l r)
                


insertSpec :: Spec
insertSpec = describe "insertion spec" $ do
    prop "Inserting from a list yields a valid tree" $
        \(xs :: [(Int,Int)]) -> case AA.checkInvariant $ AA.fromList xs of
            Left _ -> False
            _      -> True

lookupSpec :: Spec
lookupSpec = describe "lookup spec" $ do
    prop "Inserting and then looking up should behaves like inverses" $ 
        \(t :: RandomAA Int Int) ((k,v) :: (Int,Int)) -> case AA.lookup k $ AA.insert k v $ getAA t of
            Just v'  -> v == v'
            Nothing  -> False
    prop "looking up a value that's not there should result in a nothing" $
        \(Diff (t,k) :: Different Int Int) -> case AA.lookup k t of
            Nothing -> True
            Just any -> False

skewSpec :: Spec
skewSpec = describe "skew spec" $ do
    it "works as in the example" $ 
        AA.skew t == t' 
            where
                t  = Node 2 'T' 'T' (Node 2 'L' 'L' (Node 1 'A' 'A' Empty Empty) (Node 1 'B' 'B' Empty Empty )) (Node 1 'R' 'R' Empty Empty)
                t' = Node 2 'L' 'L' (Node 1 'A' 'A' Empty Empty) (Node 2 'T' 'T' (Node 1 'B' 'B' Empty Empty) (Node 1 'R' 'R' Empty Empty))

splitSpec :: Spec
splitSpec = describe "split spec" $ do 
    it "works as in the example" $ 
        AA.split t == t'
            where 
                t  = Node 1 'T' 'T' (Node 1 'A' 'A' Empty Empty) (Node 1 'R' 'R' (Node 1 'B' 'B' Empty Empty) (Node 1 'X' 'X' Empty Empty))
                t' = Node 2 'R' 'R' (Node 1 'T' 'T' (Node 1 'A' 'A' Empty  Empty ) (Node 1 'B' 'B' Empty Empty )) (Node 1 'X' 'X' Empty Empty) 


invariantSpec :: Spec
invariantSpec = describe "making sure that dani's implementation of checkInvariants behaves correctly" $ do
    prop "Inserting from a list yields a valid tree for both methods: " $
        \(xs :: [(Int,Int)]) -> case let fxs = AA.fromList xs in (AA.checkInvariant fxs,AA.checkInvariant'' fxs) of
            (Right _, Right _) -> True
            _      -> False
    
    prop "both methods should behave equally (up to error) given super random trees" $
        \(SRAA t :: SuperRandomAA Int ()) -> case (AA.checkInvariant t,AA.checkInvariant'' t) of
            (Right _, Right _) -> True
            (Left _, Left _)   -> True 
            _      -> False

internalsSpec :: IO ()
internalsSpec = traverse_ hspec [skewSpec,splitSpec,lookupSpec, insertSpec, invariantSpec]