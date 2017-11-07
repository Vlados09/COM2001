module Assignment2 where

    import Assignment1
    import System.Random
    import MergeSort

    type Player = (Hand, Int)

    -- simplerPlayer :: DomsPlayer
    hsdPlayer :: Board->Int->Hand -> (Domino, End)
    hsdPlayer b n h
        | (n /= 0) = scoreN2 b n h
        | otherwise = hsdPlayer b (n-1) h

    domsPlayer :: Hand->Board -> (Domino, End)
    domsPlayer h b = hsdPlayer b 8 h

    shuffleDoms :: Int -> Hand
    shuffleDoms n = map fst(mergesort (\(_,n1) (_,n2)->n1<n2) (zip alld rlis))
        where rlis = take 26 (randoms (mkStdGen n):: [Int])
