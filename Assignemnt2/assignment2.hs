{- |
Module      :  Assignment2
Writen By   :  Vladyslav Bondarenko
Description :  Functions to play a game of Dominoes
Stability   :  experimental
-}

module Assignment2 where

    import System.Random
    import Data.Maybe
    import Debug.Trace

    {- VARIABLE NAMES
        b - a Board
        d - a Domino
        h - a Hand
        e - an End
        pX - DomsPlayer X
        pXh/pXs - hand and score for X player
        t - turn (True if player 1, False if player 2)
        lpp/rpp - possible plays on the left/right
        gs - a game state.
    -}

    -- Datatype: Score -> represents scores for player 1 and player 2 ---------
    type Score = (Int, Int)

    -- Datatype: GameState -> represent a current state in game ---------------
    type GameState = (Board,(Hand,Hand),Score,Bool)

    -- Take a GameState and a DomsPlayer and returns a new GameState after
    -- a move has been made by the DomsPlayer.
    -- Hand is not updated as goesP doesn't allow for repeated dominoes.
    updateState :: GameState->DomsPlayer -> GameState
    updateState (b,(p1h,p2h),(p1s,p2s),t) p
        | t = (newb,(p1h,p2h),((p1s+scoreBoard newb),p2s),not t)
        | not t = (newb,(p1h,p2h),(p1s,(p2s+scoreBoard newb)),not t)
        where
            (d,e)
                | t = p p1h b
                | not t = p p2h b
            newb = fromJust(playDom d b e)

    -- Takes a Hand and a Board and returns a Domino to play and the End
    -- to play it at, i.e. each time it is called it makes a single move.
    type DomsPlayer = Hand->Board -> (Domino, End)

    -- always plays the highest scoring domino in its hand.
    hsdPlayer :: DomsPlayer
    hsdPlayer h b = bestDoms 8 h b

    -- a helper function for hsdPlayer - selects the Domino from a hand
    -- which will relust in a highest score.
    bestDoms :: Int->Hand->Board -> (Domino, End)
    bestDoms n h b
        | not$null lp = (head lp,L)
        | not$null rp = (head rp,R)
        | otherwise = bestDoms (n-1) h b
        where (ls,rs) = scoreN b n
              (lpp,rpp) = possPlays h b
              (lp,rp) = ((my_intersect ls lpp),(my_intersect rs rpp))

    -- plays the first domino in its hand which will go.
    simplePlayer :: DomsPlayer
    simplePlayer h b
        |(not$null lpp) = (head(lpp), L)
        | otherwise = (head(rpp), R)
        where (lpp,rpp) = possPlays h b

    --  Takes a random number generator and returns a list of all the dominoes
    --  in random order.
    shuffleDoms :: Int -> Hand
    shuffleDoms n = map fst(mergesort (\(_,n1) (_,n2)->n1<n2) (zip alld rlis))
        where rlis = take 18 (randoms (mkStdGen n):: [Int])

    -- A helper function for playDomsRound
    -- Recursively goes through each player making a move if possible.
    -- Return their scores at the end of the round.
    playDoms :: DomsPlayer->DomsPlayer->GameState -> (Int, Int)
    playDoms p1 p2 gs@(b,h@(p1h,p2h),s@(p1s,p2s),t)
        | (knockingP p1h b) && (knockingP p2h b) = s
        | t && knockingP p1h b = traceShow gs playDoms p1 p2 (b,h,s,not t)
        | not t && knockingP p2h b = traceShow gs playDoms p1 p2 (b,h,s,not t)
        | t = traceShow gs playDoms p1 p2 (updateState gs p1)
        | not t = traceShow gs playDoms p1 p2 (updateState gs p2)

    -- Initializes the GameState - creating new hands, etc. and passes it
    -- on to recursive function playDoms.
    playDomsRound :: DomsPlayer->DomsPlayer->Int -> (Int, Int)
    playDomsRound  p1 p2 n = playDoms p1 p2([],((take 9 h),(drop 9 h)),(0,0),True)
        where h = shuffleDoms n

    -- Checks if two lists of Dominoes have common elemements and retunrs a
    -- new list with those elements.
    -- Unlie inbuild intersect, allows for the fliped dominoes.
    my_intersect :: [Domino]->[Domino] -> [Domino]
    my_intersect [] _ = []
    my_intersect _ [] = []
    my_intersect (h1:t1) lis2
        | (elem h1 lis2) || (elem (dflip h1) lis2) = (h1:my_intersect t1 lis2)
        | otherwise = my_intersect t1 lis2

-------------------------  Code for MergeSort  --------------------------------
    --merge

    merge :: Ord a=> (a->a -> Bool)->[a]->[a] -> [a]

    merge _ [] lis2 = lis2

    merge _ lis1 [] = lis1
    merge compfn lis1 lis2
      | compfn h1 h2 = (h1:merge compfn t1 lis2)
      | otherwise = (h2:merge compfn lis1 t2)
      where
        (h1:t1)=lis1
        (h2:t2)=lis2
    -------------------------------------------------
    --mergesort

    mergesort :: Ord a=> (a->a -> Bool)->[a] -> [a]

    mergesort _ [] = [] --check this once only
    mergesort compfn dlis =
                mergesortA compfn (map (\ e -> [e]) dlis)
    -------------------------------------------------
    --mergsortA
    mergesortA :: Ord a=> (a->a -> Bool)->[[a]] -> [a]

    mergesortA _ [lis] = lis -- one list only, it's the answer
    -- general case - merge list pairs & repeat
    mergesortA compfn mlis= mergesortA compfn (mergesortpass compfn mlis)
    ---------------------------------------------------------------------
    --mergesortpass
    -- merge pairs of lists
    mergesortpass :: Ord a=> (a->a -> Bool)->[[a]] -> [[a]]

    mergesortpass _ [] = []
    mergesortpass _ [l]= [l] -- one element only, return list unchanged

    -- general case - merge first two lists, cons to remainder

    mergesortpass compfn (lis1:(lis2:rest)) =
        (merge compfn lis1 lis2): mergesortpass compfn rest

-----------------------  Code from Assignment1  --------------------------------
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
    badd d@(fd,sd) b e
        | (e == R) && (fd == snd(last b)) = b ++ [d] -- Add to the tail
        | (e == R) && (sd == snd(last b)) = b ++ [dflip d] -- Flip and add
        | (e == L) && (sd == fst(head b))= d : b -- Add to the head
        | (e == L) && (fd == fst(head b))= (dflip d) : b -- Flip and add
        | otherwise = b

    -- Sums up both ends depen
    sume :: Board -> Int
    sume [] = 0
    sume b@((fh,sh):_)
        |  doubleL && doubleR = fh + sh + fst(last b) + snd(last b) -- both ends are double
        |  doubleR = fst(last b) + snd(last b) + fh -- just Right is double
        |  doubleL = fh + sh + snd(last b) -- Just left is double
        | otherwise = fh + snd(last b) -- Both ends are single
        where doubleL = fh == sh
              doubleR = fst(last b) == snd(last b)

    -- Datatype: End ----------------------------------------------------------
    data End = L | R
                  deriving (Eq, Show)


    -- Check if a given domino can be played on a given end of a board.
    -- Return true if it can.
    goesP :: Domino->Board->End -> Bool
    goesP _ [] _ = True -- True if board is empty.
    goesP d@(fd, sd) b@((fhb,shb):_) e
        | playedP d b = False
        | (e == L) = ((fd == fhb) || (sd == fhb))
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
    scoreBoard [] = 0
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
    scoreN b n = (filter (\ d -> (scoreBoard(badd d b L)) == n) fpp,
                  filter (\ d -> (scoreBoard(badd d b R)) == n) spp)
        where (fpp,spp) = possPlays alld b
