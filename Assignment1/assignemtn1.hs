

module Dominoes where

    alld = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),
                         (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                         (2,2),(2,3),(2,4),(2,5),(2,6),
                         (3,3),(3,4),(3,5),(3,6),
                         (4,4),(4,5),(4,6),
                         (5,5),(5,6),
                         (6,6)]

    type Domino = (Int, Int)

    dcreate :: Int->Int -> Domino
    dcreate a b
        | (a <= 6 && b <= 6) = (a, b)

    dsame :: Domino->Domino -> Bool
    dsame d1 d2
        |((fst d1 == fst d2) && (snd d1 == snd d2)) ||
        ((fst d1 == snd d2) && (snd d1 == fst d2)) = True
        | otherwise = False

    dflip :: Domino -> Domino
    dflip d = (snd d, fst d)

    type Hand = [Domino]

    hcreate :: Hand
    hcreate = []

    hadd :: Domino->Hand -> Hand
    hadd d h = h ++ [d]

    type Board = [Domino]

    bcreate :: Board
    bcreate = []

    badd :: Domino->Board->End -> Board
    badd d [] _ = [d]
    badd d b e
        | (e == 'R') && (fst d == snd(last b)) = b ++ [d]
        | (e == 'R') && (snd d == snd(last b)) = b ++ [dflip d]
        | (e == 'L') && (snd d == fst(head b))= d : b
        | (e == 'L') && (fst d == fst(head b))= (dflip d) : b

    type End = (Char)

    ecreate :: Char -> End
    ecreate c = (c)

    goesP :: Domino->Board->End -> Bool
    goesP _ [] _ = True
    goesP d b e
        | (e == 'L') = ((fst d == fst(head b)) || (snd d == fst(head b)))
        | (e == 'R') = ((fst d == snd(last b)) || (snd d == snd(last b)))
        | otherwise = False

    knockingP :: Hand->Board -> Bool
    knockingP _ [] = False
    knockingP [] _ = True
    knockingP (h:t) b
        | ((goesP h b 'L') || (goesP h b 'R')) = False
        | otherwise = knockingP t b

    playedP :: Domino->Board -> Bool
    playedP _ [] = False
    playedP d (h:t)
        | (dsame d h) = True
        | otherwise = playedP d t

    possPlays :: Hand->Board -> ([Domino], [Domino])
    possPlays h [] = (h, h)
    possPlays h b = (filter (\d -> ((goesP d b 'L') && (not(playedP d b))))  h,
                                filter (\d -> ((goesP d b 'R') && (not(playedP d b)))) h)

    playDom :: Domino->Board->End -> Maybe Board
    playDom d b e
        | (goesP d b e)  = Just (badd d b e)
        | otherwise = Nothing

    scoreBoard ::  Board -> Int
    scoreBoard b
        | ((ends `mod` 5) == 0) = ends `div` 5
        | ((ends `mod` 3) == 0) = ends `div` 3
        | ((ends `mod` 3) == 0 && ((ends `mod` 5) == 0)) = ends `div` 3 + ends `div` 5
        | otherwise = 0
        where ends = (fst(head b)) + (snd(last b))

    scoreN :: Board->Int -> ([Domino], [Domino])
    scoreN b n = (filter (\d -> (scoreBoard(badd d b 'L'))==n) (fst(possPlays alld b)),
                           filter (\d -> (scoreBoard(badd d b 'R'))==n) (snd(possPlays alld b)))
