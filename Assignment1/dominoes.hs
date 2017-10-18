module Dominoes where

    type Domino = [(Int, Int)]

    dcreate :: Domino
    dcreate = []

    ddefine :: Int->Int -> Domino
    ddefine a b = [(a,b)]

    type Hand = [(Domino)]

    hcreate :: Hand
    hcreate = []

    hput :: Domino->Hand -> Hand
    hput d h = h : d

    type Board = [(Domino)]

    bcreate :: Board
    bcreate = []

    type End = [(Char)]

    ecreate :: End
    ecreate = []

    edefine :: Char -> End
    edefine c = [(c)]

    goesP :: Domino -> Board -> End ->Bool
    goesP d b e
        | null b = True
        | (e == L) = ((fst d == fst head b) || (snd d == fst head b))
        | (e == R) = ((fst d == last tail b) || (snd d == last tail b))
