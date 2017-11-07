{- |
Module      :  Assignment1
Writen By   :  Vladyslav Bondarenko
Description :  Data types and simple functions for domino game.
Stability   :  experimental
-}

module Assignment1 where

    {- VARIABLE NAMES
        b a Board
        d a Domino
        h a Hand
        e an End
        fd left pip of a domino
        sd right pip of a domino
    -}

    -- All the possible dominoes
    alld = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),
                (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                (2,2),(2,3),(2,4),(2,5),(2,6),
                (3,3),(3,4),(3,5),(3,6),
                (4,4),(4,5),(4,6),
                (5,5),(5,6),
                (6,6)]

    -- Datatype: Domino -> a tuple pair of ints. ------------------------------
    type Domino = (Int, Int)

    -- Create a new Domino with sides in range of 0..6
    dcreate :: Int->Int -> Domino
    dcreate a b
        | (a <= 6 && b <= 6 && a >= 0 && b >= 0) = (a, b)

    -- Check if two dominoes are the same
    -- Returng true if they are.
    dsame :: Domino->Domino -> Bool
    dsame (fd1, sd1) (fd2, sd2)
        | (fd1 == fd2 && sd1 == sd2) ||
          (fd1 == sd2 && sd1 == fd2) = True
        | otherwise = False

    -- Flip a given domino(d)
    dflip :: Domino -> Domino
    dflip d = (snd d, fst d)

    -- Datatype: Hand -> a list of Dominoes -----------------------------------
    type Hand = [Domino]

    -- Create a new hand
    hcreate :: Hand
    hcreate = []

    -- Add a domino(d) to a given hand(h)
    -- Returns a new hand.
    hadd :: Domino->Hand -> Hand
    hadd d h = h ++ [d]

    -- Datatype: Board -> a list of Dominoes ----------------------------------
    type Board = [Domino]

    -- Create a new board
    bcreate :: Board
    bcreate = []

    -- Add a domino(d) to a board(b) on a given end(e)
    -- Returns a new board.
    badd :: Domino->Board->End -> Board
    badd d [] _ = [d]
    badd d b e
        | (e == R) && (fst d == snd(last b)) = b ++ [d] -- Add to the tail
        | (e == R) && (snd d == snd(last b)) = b ++ [dflip d] -- Flip and add
        | (e == L) && (snd d == fst(head b))= d : b -- Add to the head
        | (e == L) && (fst d == fst(head b))= (dflip d) : b -- Flip and add
        | otherwise = b

    -- Sums up both ends depen
    sume :: Board -> Int
    sume [] = 0
    sume (h:t)
        |  doubleL && doubleR = fst(h) + snd(h) +
                                                  fst(last t) + snd(last t) -- both ends are double
        |  doubleR = fst(last t) + snd(last t) + fst(h) -- just Right is double
        |  doubleL = fst(h) + snd(h) + snd(last t) -- Just left is double
        | otherwise = fst(h) + snd(last t) -- Both ends are single
        where { doubleL = fst(h) == snd(h) ;
                      doubleR = fst(last t) == snd(last t) }

    -- Datatype: End ----------------------------------------------------------
    data End = L | R
                      deriving (Eq)

    -- Check if a given domino can be played on a given end of a board.
    -- Return true if it can.
    goesP :: Domino->Board->End -> Bool
    goesP _ [] _ = True -- True if board is empty.
    goesP (fd, sd) b e
        | (e == L) = ((fd == fst(head b)) || (sd == fst(head b)))
        | (e == R) = ((fd == snd(last b)) || (sd == snd(last b)))
        | otherwise = False

    -- Check if any dominoes in a hand that can be played on a given board.
    -- Return True if there are none.
    knockingP :: Hand->Board -> Bool
    knockingP _ [] = False -- False if board is empty.
    knockingP [] _ = True -- True if hand is empty(to end recursion).
    knockingP (h:t) b
        | ((goesP h b L) || (goesP h b R)) = False
        | otherwise = knockingP t b -- Recurse over the hand.

    -- Сheck if a given Domino has already been played.
    -- Return True if it has.
    playedP :: Domino->Board -> Bool
    playedP _ [] = False -- Flase if board is empty.
    playedP d (h:t)
        | (dsame d h) = True
        | otherwise = playedP d t -- Recurse over the board.

    -- Return a pair of lists of Dominoes from a hand that can be played.
    -- Only retuns dominoes that haven not been played yet.
    possPlays :: Hand->Board -> ([Domino], [Domino])
    possPlays h [] = (h, h) -- All dominoes can be played if board is empty.
    possPlays h b = (filter (\ d -> ((goesP d b L) && (not(playedP d b))))  h,
                               filter (\ d -> ((goesP d b R) && (not(playedP d b)))) h)

    -- Try to place a given domino on a board at a given end.
    -- Return a Maybe Board.
    playDom :: Domino->Board->End -> Maybe Board
    playDom d b e
        | (goesP d b e)  = Just (badd d b e)
        | otherwise = Nothing

    -- Take a board and count its current score.
    scoreBoard ::  Board -> Int
    scoreBoard b
        | ((ends `mod` 3) == 0 && ((ends `mod` 5) == 0)) =
            ends `div` 3 + ends `div` 5
        | ((ends `mod` 5) == 0) = ends `div` 5
        | ((ends `mod` 3) == 0) = ends `div` 3
        | otherwise = 0
        where ends = sume b

    -- Return the pair of lists of dominoes that can be played that will result
    -- in a given score n.
    scoreN :: Board->Int -> ([Domino], [Domino])
    scoreN b n = (filter (\ d -> (scoreBoard(badd d b L)) == n)
                          (fst(possPlays alld b)), -- A list of possible plays on 'L'
                          filter (\ d -> (scoreBoard(badd d b R)) == n)
                          (snd(possPlays alld b))) -- A list of possible plays on 'R'
