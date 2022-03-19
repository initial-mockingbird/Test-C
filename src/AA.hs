module AA 
    ( AA
    , empty 
    , isEmpty
    , insert
    , lookup
    , delete 
    , member
    , fromList
    )
    where

import System.IO 
import System.Random (randomRIO) -- BORRAR 
import Prelude hiding (lookup)

---------------------------------
-- Types                        |
---------------------------------

data AA k a 
    = Empty
    | Node { lvl :: Int
    , key :: k
    , val :: a
    , lAA :: AA k a
    , rAA :: AA k a
    } deriving (Eq)
    
---------------------------------
-- Instances                    |
---------------------------------

instance Functor (AA k) where
    fmap f Empty                    = Empty 
    fmap f (Node l k v lNode rNode) = Node l k (f v) (fmap f lNode) (fmap f rNode)

instance Foldable (AA k) where
    foldr _ acc Empty = acc
    foldr f acc (Node _ _ v left right) = foldr f acc' left 
        where
            acc' = f v $ foldr f acc right  


foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> AA k a -> b
foldrWithKey _ acc Empty = acc
foldrWithKey f acc (Node _ k v left right) = foldrWithKey f acc' left 
        where
            acc' = f k v $ foldrWithKey f acc right  

---------------------------------
-- Builder                      |
---------------------------------

empty :: Ord k => AA k a
empty = Empty

insertWithKey :: Ord k => k -> a -> (k -> a -> a -> a) -> AA k a -> AA k a
insertWithKey k v _ Empty = Node 1 k v Empty Empty 
insertWithKey k v f t@Node{key=_key,val=_val,lAA=l,rAA=r}
    | k < _key  = split . skew $ t{lAA =insertWithKey k v f l}
    | k > _key  = split . skew $ t{rAA =insertWithKey k v f r}
    | otherwise = t{val= f k v _val}

insertWith :: Ord k => k -> a -> (a -> a -> a) -> AA k a -> AA k a
insertWith k v f = insertWithKey k v (\_ v v' -> f v v') 

insert :: Ord k => k -> a -> AA k a -> AA k a
insert k v = insertWith k v const

delete = undefined 

lookup :: Ord k => k -> AA k v -> Maybe v
lookup _ Empty = Nothing 
lookup k Node{key=_key,val=_val,lAA=_lAA,rAA=_rAA}
    | k < _key  = lookup k _lAA
    | k > _key  = lookup k _rAA
    | otherwise = Just _val 

toList :: Ord k => AA k v -> [(k,v)]
toList = foldrWithKey (\k v acc -> (k,v):acc) []

fromList :: Ord k => [(k,v)] -> AA k v
fromList = foldr (\(k,v) t -> insert k v t) empty

member :: Ord k => k -> AA k v -> Bool
member k t = case lookup k t of 
  Nothing -> False 
  Just _  -> True

---------------------------------
-- Functions                    |
---------------------------------


isEmpty :: AA k a -> Bool
isEmpty Empty = True
isEmpty _     = False 

---------------------------------
-- Internal                     |
---------------------------------

skew :: Ord k => AA k v -> AA k v
skew Empty = Empty
skew t@Node{lAA=Empty} = t
skew t@(Node lvl _ _ l@Node{lvl=lvlL, rAA=rAAL} _) 
    | lvlL == lvl = l{rAA = t{lAA=rAAL}}
    | otherwise   = t 

split :: Ord k => AA k v -> AA k v 
split Empty = Empty
split t@Node{rAA=Empty} = t
split t@Node{rAA=Node{rAA=Empty}} = t
split t@(Node lvl key val lAA r@Node{lvl=lvlR, lAA=lAAR,rAA=Node{lvl=lvlRR}}) 
    | lvl <= lvlRR = r{lvl=lvlR + 1, lAA=t{rAA=lAAR}}
    | otherwise  = t

decreaseLevel :: Ord k => AA k v -> AA k v
decreaseLevel Empty = Empty
decreaseLevel t@Node{lvl=_lvl,lAA=l,rAA=r}
    | shouldBe < _lvl = t{lvl=shouldBe,rAA=newRight} 
    | otherwise       = t 
    where
        getLevel Empty = 0
        getLevel Node {lvl=_lvl} = _lvl 

        newRight = if shouldBe < getLevel r then setLevel shouldBe r else r

        shouldBe = min (getLevel l) (getLevel r) + 1

        setLevel _ Empty = Empty
        setLevel n t     = t{lvl = n}

---------------------------------
-- Tipos invaiante              |
---------------------------------

data Direction = L | R deriving (Eq,Show)

type Crumbs = [Direction]

data InvariantError = BadLeafLevel | BadLeftLevel | BadRightLevel | BadGrandLevel | OneChild deriving (Eq,Show)

type VTree k a =  Either (Crumbs,InvariantError) (AA k a)


---------------------------------
-- Auxiliar func Invariant      |
---------------------------------

combineVTree :: Direction -> VTree k v -> VTree k v
combineVTree _ (Right v) = Right v
combineVTree dir (Left (lista,err)) = Left ( [dir]++lista, err )

---------------------------------
-- Invariant                    |
---------------------------------

checkInvariant :: Ord k => AA k v -> VTree k v
checkInvariant nodo = checkInvariant' nodo Empty

checkInvariant' :: Ord k => AA k v -> AA k v -> VTree k v
checkInvariant' Empty grandP = Right $ Empty
-- Casos lvl = 1
checkInvariant' (Node lvl key val Empty Empty) _ 
    | lvl == 1 = Right $ (Node 1 key val Empty Empty)
    | otherwise = Left $ ([],OneChild) 
-- lvl>1
checkInvariant' (Node l k v lNode rNode) grandP
    | l>1 && ( (isEmpty lNode) || (isEmpty rNode) )
        = Left $ ([],OneChild) 
    | not ( isEmpty lNode ) && ( lvl lNode ) /= (l-1) 
        = Left $ ([],BadLeftLevel) 
    | not ( isEmpty rNode ) && ( lvl rNode ) > l 
        = Left $ ([],BadRightLevel)
    | not ( isEmpty grandP ) && ( (lvl grandP) <= (lvl rNode) ) 
        = Left $ ([R], BadGrandLevel)
    | otherwise = do 
        lNode2 <- combineVTree L $ checkInvariant' lNode cuNode 
        rNode2 <- combineVTree R $ checkInvariant' rNode cuNode 
        Right cuNode 
    where cuNode = Node l k v lNode rNode


