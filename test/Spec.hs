import  AA.AASpec  
import Match.MatchSpec
import Data.Foldable (sequenceA_)

main :: IO ()
main = sequenceA_
    [ internalsSpecMatch
    , internalsSpecAA
    ]
