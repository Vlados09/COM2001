module Dominoes where

    type Domino = [Int, Int]

    dcreate :: Domino
    dcreate = []

    ddefine :: Int->Int -> Domino
    ddefine a b = [a,b]

    dsame :: Domino->Domino -> Bool
    dsame d1 d2
        | (fst d1 == fst d2) && (snd d1 == snd d2) = True
        | otherwise = False

    type Hand = [(Domino)]

    hcreate :: Hand
    hcreate = []

    hput :: Domino->Hand -> Hand
    hput d h = h : d

    type Board = [(Domino)]

    bcreate :: Board
    bcreate = []

    type End = [Char]

    ecreate :: End
    ecreate = []

    edefine :: Char -> End
    edefine c = [c]

    goesP :: Domino->Board->End -> Bool
    goesP _ [] _ = True
    goesP d b e
        | (e == L) = ((fst d == fst head b) || (snd d == fst head b))
        | (e == R) = ((fst d == last tail b) || (snd d == last tail b))
        | otherwise = False

    knockingP :: Hand->Board -> Bool
    knockingP _ [] = True
    knockingP [] _ = False
    knockingP (h:t) b
        | (goesP h b L) || (goesP h b R) = True
        | otherwise knockingP t b

    playedP :: Domino->Board -> Bool
    playedP _ [] = False
    playedP d (h:t)
            | (dsame d h) = True
            | otherwise playedP d t
