module PlayDominoes where

    import Dominoes
    import System.Random

    type Player = (Hand, Int)

    simplerPlayer :: DomsPlayer
    hsdPlayer :: DomsPlayer

    --DomsPlayer :: Hand->Board ->[Domino, End]

    shuffleDoms :: Int -> Hand
    shuffleDoms n = mergersort (\(_,n1) (_,n2)->n1<n2) (zip alld rlis)
        where rlis = take 26 (randoms (mkStdGen n):: [Int])
