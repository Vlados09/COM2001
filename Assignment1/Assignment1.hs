{- |
Module      :  Assignment1
Writen By   :  Vladyslav Bondarenko
Description :  Data types and simple functions for domino game.
Stability   :  experimental
-}

module Dominoes where

    -- Generate all the possible dominoes
    alld = [(a,b)] a <- [0..6], b <- [0..6]

    -- Datatype: Domino -> a tuple pair of ints.
    type Domino = (Int, Int)
    -- Create a new Domino with sides in range if 0..6
    dcreate :: Int->Int -> Domino
    dcreate a b
        | (a <= 6 && b <= 6 && a >= 0 && b >= 0) = (a, b)
    -- Check if two dominoes are the same
    -- Returng true if they are.
    dsame :: Domino->Domino -> Bool
    dsame (fd, sd) d2
        |((fst d1 == fst d2) && (snd d1 == snd d2)) ||
        ((fst d1 == snd d2) && (snd d1 == fst d2)) = True
        | otherwise = False
    -- Flip a given domino
    dflip :: Domino -> Domino
    dflip d = (snd d, fst d)

    -- Datatype: Hand -> a list of Dominoes
    type Hand = [Domino]
    -- Create a new hand
    hcreate :: Hand
    hcreate = []
    -- Add a domino to a given hand
    -- Returns a new hand.
    hadd :: Domino->Hand -> Hand
    hadd d h = h ++ [d]

    -- Datatype: Board -> a list of Dominoes
    type Board = [Domino]
    -- Create a new board
    bcreate :: Board
    bcreate = []
    -- Add a domino to a board on a given end
    -- Returns a new board.
    badd :: Domino->Board->End -> Board
    badd d [] _ = [d]
    badd d b e
        | (e == 'R') && (fst d == snd(last b)) = b ++ [d] -- Add to the tail
        | (e == 'R') && (snd d == snd(last b)) = b ++ [dflip d] -- Flip and add
        | (e == 'L') && (snd d == fst(head b))= d : b -- Add to the head
        | (e == 'L') && (fst d == fst(head b))= (dflip d) : b -- Flip and add

    -- Datatype: End -> a tuple with a single char.
    type End = (Char)
    -- Create a new end (either L(eft) or R(right))
    ecreate :: Char -> End
    ecreate c
      | (c == 'L' || c == 'R') = (c)

    -- Check if a given domino can be played on a given end of a board.
    -- Return true if it can.
    goesP :: Domino->Board->End -> Bool
    goesP _ [] _ = True -- True if board is empty.
    goesP d b e
        | (e == 'L') = ((fst d == fst(head b)) || (snd d == fst(head b)))
        | (e == 'R') = ((fst d == snd(last b)) || (snd d == snd(last b)))
        | otherwise = False

    -- Check if any dominoes in a hand that can be played on a given board.
    -- Return True if there are none.
    knockingP :: Hand->Board -> Bool
    knockingP _ [] = False -- False if board is empty.
    knockingP [] _ = True -- True if hand is empty(to end recursion).
    knockingP (h:t) b
        | ((goesP h b 'L') || (goesP h b 'R')) = False
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
    possPlays h b = (filter (\ d -> ((goesP d b 'L') && (not(playedP d b))))  h,
                    filter (\ d -> ((goesP d b 'R') && (not(playedP d b)))) h)

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
        where ends = (fst(head b)) + (snd(last b))

    -- Return the pair of lists of dominoes that can be played that will result
    -- in a given score n.
    scoreN :: Board->Int -> ([Domino], [Domino])
    scoreN b n = (filter (\ d -> (scoreBoard(badd d b 'L')) == n)
                 (fst(possPlays alld b)), -- A list of possible plays on 'L'
                  filter (\ d -> (scoreBoard(badd d b 'R')) == n)
                  (snd(possPlays alld b))) -- A list of possible plays on 'R'
