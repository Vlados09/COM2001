{- |
Module      :  Assignment3
Writen By   :  Vladyslav Bondarenko
Description :  An incomplete intelligent player for Dominoes game.
Stability   :  experimental
-}

module IntelligentPlayer where

    import DomsMatch
    import Debug.Trace

    {- VARIABLE NAMES
        b/db - a DomBoard
        d - a Domino
        h - a Hand
        e - an End
        his - histroy
        possed - possible enemy Domniones
        probed - probable enemy Dominoes
        ld/rd - doms to go on the left/right
        sld/srd - safe left/right doms
    -}
    -- Datatype: Tactic -> take a hand, a board, player and scores and returns
    -- a tuple with a bool representing where as tactic is successful, dom ----
    -- and end to play it on --------------------------------------------------
    type Tactic = Hand->DomBoard->Player->Scores -> (Bool,Dom,End)

    -- Returns a list of chosen tacktics representing each player I tested.
    -- WARNING: Has to alway end with a hsPlay!!!
    tactics :: Int -> [Tactic]
    tactics n
        | n == 1 = [hsPlay]
        | n == 2 = [safeDomPlay, hsPlay]
        | n == 3 = [majority1Play,safeDomPlay,hsPlay]
        | n == 4 = [playToN 61,majority1Play,safeDomPlay,hsPlay]
        | n == 5 = [playToN 61,preventWinPlay,majority1Play,safeDomPlay,hsPlay]
        | n == 6 = [playToN 61,preventWinPlay,playToN 59,majority1Play,safeDomPlay,hsPlay]
        | n == 7 = [play54,playToN 61,preventWinPlay,playToN 59,majority1Play,safeDomPlay,hsPlay]

    -- Representation for a failed tactic for an intelligent player to move
    -- onto another tactic.
    falseTactic :: (Bool,Dom,End)
    falseTactic = (False,(-1,-1),R)

    ----------------------------------------------------------------------------
    -- intelligentPlayer representing a DomsPlayer
    -- Chooses a list of tactics for the current player.
    intelligentPlayer1 :: DomsPlayer
    intelligentPlayer1 p = intelligent (tactics 7) p

    intelligentPlayer2 :: DomsPlayer
    intelligentPlayer2 p = intelligent (tactics 1) p

    -- Take a list of tactics and iterates over them until a good tactic to use
    -- is found. Returns a dominoe and an end.
    intelligent :: [Tactic]->DomsPlayer
    intelligent (tactic:xs) h b p s
        | cond = (d,e)
        | otherwise = intelligent xs h b p s
        where (cond,d,e) = tactic h b p s

    ---------------------------- Tactics -----------------------------------

    -- Plays (5,4) domino in the begining of a game it is in the hand.
    play54 :: Tactic
    play54 h InitBoard p s
        | elem (5,4) h || elem (4,5) h = (True, (5,4), L)
        | otherwise = falseTactic
    play54 h b p s = falseTactic

    -- Play a higest scoring domino.
    hsPlay :: Tactic
    hsPlay h b _ _ = (True, d, e)
        where (d,e,_) = hsd h b

    -- Plays a safe domino.
    safeDomPlay :: Tactic
    safeDomPlay h b p s
        |not(null sd) = hsPlay sd b p s
        |otherwise = falseTactic
        where
            (ld,rd) = safeDoms h b p s
            sd = ld++rd

    -- Play to achive a given score.
    -- Either input should be 59 or 61.
    playToN :: Int->Tactic
    -- Return false if current aim is out of range even with the highest scoring
    -- domino.
    playToN n _ _ p s
        | n == 61 && ps < 53 = falseTactic
        | n == 59 && (ps < 51 && ps >= 59) = falseTactic
        where ps = scorePlayer p s

    playToN n h b p s
        |not (null ldw) = traceShow (h,b,p,s) (True,(head ldw),L)
        |not (null rdw) = traceShow (h,b,p,s) (True,(head rdw),R)
        |otherwise = falseTactic
        where
            (ld,rd) = ((leftdrops h b),(rightdrops h b))
            ldw = filter (\d -> ((scoreDom d L b)+(scorePlayer p s))==n) ld
            rdw = filter (\d -> ((scoreDom d R b)+(scorePlayer p s))==n) rd

    -- Atempts to prevent opponent from wining by analising his possible and
    -- probable dominoes. Currently, only works if enemy has only one possible
    -- or one probable wining domino.
    preventWinPlay :: Tactic
    preventWinPlay _ _ p s
        | scoreEnemy p s < 53 = falseTactic

    preventWinPlay h b p s
        | (possl == 0) = falseTactic
        | (possl == 1) = preventWin e h b p s
        | (probl == 1) = preventWin e h b p s
        | otherwise = falseTactic
        where
            his = getHistory b
            possed = possEnemyDoms h his p
            probed = probEnemyDoms possed his p
            possewd = enemyWinDoms possed b p s
            probewd = enemyWinDoms probed b p s
            possl = length possewd
            probl = length probewd
            (_,e)
                | (possl == 1) = head possewd
                | (probl == 1) = head probewd

    -- A helper function for preventWinPlay. Takes an end at which opponent
    -- would play a winning domino and play a safe domino on that end.
    preventWin :: End -> Tactic
    preventWin e h b p s
        | e == L && not(null ld) = (True,head ld,L)
        | e == R && not(null rd) = (True,head rd,R)
        | otherwise = falseTactic
        where
            (ld,rd) = safeDoms h b p s

    -- Plays a safe domino with a pip number of which there is more than 5 in
    -- current hand.
    majority1Play :: Tactic
    majority1Play h b p s
        | n < 5 = falseTactic
        | not(null sd) = hsPlay sd b p s
        | otherwise = falseTactic
        where
            (mpip,n) = majorityPip 6 (-1,-1) h
            (sdl,sdr) = safeDoms h b p s
            ld1 = filter (\(lp,_) -> lp == mpip) sdl
            ld2 = filter (\(_,rp) -> rp == mpip) sdl
            rd1 = filter (\(lp,_) -> lp == mpip) sdr
            rd2 = filter (\(_,rp) -> rp == mpip) sdr
            sd = (ld1++ld2)++(rd1++rd2)

    -- helper function for majorityPipPlay.
    -- Parameters: Int -> current pip (i.e: from 0 to 6)
    -- (Int,Int) -> containing pip of which there is most in the hand and
    -- number of dominoes with tha pip, and returns that at the end.
    majorityPip :: Int->(Int,Int)->Hand -> (Int,Int)
    majorityPip pip (maxpip,n) h
        | pip == -1 = (maxpip,n)
        | lpipd > n = majorityPip (pip-1) (pip,lpipd) h
        | otherwise = majorityPip (pip-1) (maxpip,n) h
        where
            pipd = filter (\(lp,rp) -> lp == pip || rp == pip) h
            lpipd = length pipd

    ------------------- Other Helper Functions ---------------------------------

    -- Looks at given dominoes represented by a hand and determines which ones
    -- of those are safe to play at which end. Returns a tuple with two lists
    -- dominoes to plya on left and right repsectively.
    safeDoms :: Hand->DomBoard->Player->Scores -> ([Dom],[Dom])
    safeDoms h db p s = (sld,srd)
        where
            ld = leftdrops h db
            rd = rightdrops h db
            his = getHistory db
            possed = possEnemyDoms h his p
            probed = probEnemyDoms possed his p
            sld = filter(\d -> safeDom (d,L) probed h db p s) ld
            srd = filter(\d -> safeDom (d,R) probed h db p s) rd

    -- Determines where as given domino is safe to play. Dominoes that are not safe are
    -- those ones that can result in enemy winning, playing a higher scoring
    -- domino or forcing current player to knock.
    -- Two hands are probable enemy hand and current players hand respectively.
    -- Returns True if given domino is safe.
    safeDom :: (Dom,End)->Hand->Hand->DomBoard->Player->Scores -> Bool
    safeDom (d,e) probed h db p s
        | p2s == 61 = False
        | p2ds > p1ds = False
        | null ld && null rd = False
        | otherwise = True
        where
            p1ds = scoreDom d e db
            newdb = updateBoard d e p db
            (p2d,p2e,p2ds) = hsd probed db -- Assumers opponent is hsdPlayer
            p2s = (scoreEnemy p s) + p2ds
            ld = filter (\l -> l /= d) (leftdrops h db)
            rd = filter (\r -> r /= d) (rightdrops h db)

    -- Returns a list of dominoes and ends that will result in a win for an enemy.
    enemyWinDoms :: Hand->DomBoard->Player->Scores -> [(Dom,End)]
    enemyWinDoms [] b p s = []
    enemyWinDoms h@(_:t) b p s
        | cond = (d,e) : enemyWinDoms t b p s
        | otherwise = []
        where (cond,d,e) = playToN 61 h b p s

    -- Finds which dominoes enemy player doesn't have by examining the current
    -- board, your current hand and moves on which opponent knocked.
    possEnemyDoms :: Hand->History->Player -> Hand
    possEnemyDoms h his p = pd
        where
            b = boardFromHistory his []
            pdh1 = filter (\d -> not(elem d h)) domSet
            pdh = filter (\(fp,sp) -> not(elem (sp,fp) h)) pdh1
            pdb1 = filter (\d -> not(elem d b)) pdh
            pdb = filter (\(fp,sp) -> not(elem (sp,fp) b)) pdb1
            phis = filter (\(_,p2,_)-> p2 == p) his
            km = knockedMoves phis his
            mp = missingPips his km
            pd = filter (\(fp,sp) -> not(elem fp mp) && not(elem sp mp)) pdb

    -- Finds which dominoes enemy player might not have by looking at hise
    -- possible hand and exmaining history to look if any of the hand doms
    -- would result in a higher score than the dom enemy actually played.
    -- Returns a hand without the dominoes with better outcomes.
    probEnemyDoms :: Hand->History->Player -> Hand
    probEnemyDoms h [] _ = h
    probEnemyDoms [] _ _ = []
    probEnemyDoms h his@((d,p2,m):xs) p1
        | p2 /= p1 && ds2 > ds = probEnemyDoms newh xs p1
        | otherwise = probEnemyDoms h xs p1
        where
            (ld,rd) = historyEndDoms his (m-1)
            (ld2,rd2) = historyEndDoms his m
            db
                | ld == (-1,-1) = InitBoard
                | otherwise = (Board ld rd his)
            db2 = (Board ld2 rd2 his)
            (d2,e,ds2) = hsd h db -- Assumes opponent is hsdPlayer
            ds = scoreboard db2
            newh = filter(\dx -> dx /= d2) h

    -- Takes a History and a list of moves after which opponent knocked and
    -- returns a list of pips which opponent doesn't have.
    missingPips :: History->[Int] -> [Int]
    missingPips _ [] = []
    missingPips his (x:xs) = [lp,rp]++missingPips his xs
        where
            ((lp,_),(_,rp)) = historyEndDoms his x

    -- Takes a history for a current player and a full histroy for both plyers.
    -- Returns a list of move numbers after which opponent knocked, by
    -- looking at where current player played two moves in a row.
    knockedMoves :: History -> History -> [Int]
    knockedMoves [] _ = []
    knockedMoves ((_,p1,m):t) fhis
        | null nhis = knockedMoves t fhis
        | p1 == p2 = [m]++knockedMoves t fhis
        | otherwise = knockedMoves t fhis
        where
            nhis = filter(\(_,_,m2) -> m2 == m+1) fhis
            (_,p2,_)
                | length nhis == 1 = head nhis

    --------------- Basic Datatype manipulation functions ---------------------

    -- Takes current player and scores, returning his current score.
    scorePlayer :: Player->Scores -> Int
    scorePlayer p (p1s,p2s)
        | p==P1 = p1s
        | otherwise = p2s

    -- Takes current player and score, returning enemy current score.
    scoreEnemy :: Player->Scores -> Int
    scoreEnemy p (p1s,p2s)
        | p==P1 = p2s
        | otherwise = p2s

    -- Returns just a history from a given DomBoard
    getHistory :: DomBoard -> History
    getHistory InitBoard = []
    getHistory (Board _ _ his) = his

    -- Takes a history and returns dominoes at the ends at a given move number.
    historyEndDoms :: History -> Int -> (Dom,Dom)
    historyEndDoms his n = d
        where
            chis = filter(\(_,_,m)-> m <= n) his
            bhis = boardFromHistory chis []
            d
                | not(null bhis) = (head bhis, last bhis)
                | otherwise = ((-1,-1),(-1,-1))

    -- Takes a history and turns it into an ordered list of Dominnoes(i.e: Board)
    boardFromHistory :: History->[Dom] -> [Dom]
    boardFromHistory [] b = b
    boardFromHistory ((d@(fd,sd),_,_):t) b = boardFromHistory t (b ++ [d])
