{- 
   

   Author: Tamanna Mishra 

   Summary of file contents:
   This file consists of a program to simulate Fives-and-Threes Dominoes with 2 variants of a player - simple and smart. 
   The smart player makes use of strategies to beat it's opponent in the game whereas the simple player doesn't use
   any particular strategies but rather plays whichever tile can be played. There are comments in this file that outline
   the purpose of each function and outline the strategies implemented for the smart player.
    
    DomsMatch: code to play a dominoes match between two players.
    
    The top level function is domsMatch - it takes five arguments:
        games - the number of games to play
        target - the target score to reach
        player1, player2 - two DomsPlayer functions, representing the two players
        seed - an integer to seed the random number generator
    The function returns a pair showing how many games were won by each player.

    The functions of type DomsPlayer must take four arguments:
        The current Hand
        The current Board
        The Player (which will be one of P1 or P2)
        The current Scores
    The function returns a tuple containing the Domino to play and End to play it on.

    Stub with types provided by Emma Norling (October 2023).

 -}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)

    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player --
                  | turn == P1 && not (domInHand domino hand1) =   Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (domInHand domino hand2) =  Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type

    {- domInHand: check if a particular domino is contained within a hand -}
    domInHand :: Domino -> Hand -> Bool
    domInHand (l,r) hand = [ 1 | (dl, dr) <- hand, (dl == l && dr == r) || (dr == l && dl == r) ] /= []

    {- scoreBoard: given a board state and a boolean value indicating whether 
    the domino just played is the last one in hand, scoreBoard returns an 
    integer indicating the points awarded for the board state. -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState _  = 0
    scoreBoard (State (a,b) (c,d) _) lastTile =
      let score
            | (a == b && c == d) = calcScore (a+b+c+d)
            | (a == b) = calcScore (a+b+d)
            | (c == d) = calcScore (c+d+a)
            | otherwise = calcScore (a+d)
        in if lastTile then score + 1 else score


    {- blocked: returns true if there's no domino in hand that can be played on the given board state -}
    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False
    blocked [] _ = True
    blocked (tile:tiles) (State (a,b) (c,d) history)
      | canPlay tile L (State (a,b) (c,d) history) || canPlay tile R (State (a,b) (c,d) history) = False
      | otherwise = blocked tiles (State (a,b) (c,d) history)


    {- playDom: Given the player, a domino to be played, current board state, 
    the end to play the domino at, playDom checks if the domino can be played 
      at the given end and plays it, if possible, and returns the updated Board state -}

    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player (a,b) InitState _ = Just (State (a,b) (a,b) [((a,b), player, 1)])
    playDom player (a,b) (State (x,y) (m,n) history) L
      |a == x = Just (State (b,a) (m,n) (history ++ [ ( (a, b), player, moveNum + 1 ) ]) )
      |b == x = Just (State (a,b) (m,n) (history ++ [ ( (b, a), player, moveNum + 1 ) ]) )
      |otherwise = Nothing
      where moveNum = length history

    playDom player (a,b) (State (x,y) (m,n) history) R
      |b == n = Just (State (x,y) (b,a) (history ++ [ ( (a, b), player, moveNum + 1 ) ]) )
      |a == n = Just (State (x,y) (a,b) (history ++ [ ( (b, a), player, moveNum + 1 ) ]) )
      |otherwise = Nothing
      where moveNum = length history

{- calcScore: given the sum of the pips on the outermost tiles 
on the board, calcScore returns the score in terms of the 
fives-and-threes dominoes scoring scheme. -}

    calcScore :: Int -> Int
    calcScore num
      | num == 15 = 8
      | num `mod` 3 == 0 && num `mod` 5 == 0 = num `div` 3 + num `div` 5
      | num `mod` 3 == 0 = num `div` 3
      | num `mod` 5 == 0 = num `div` 5
      | otherwise = 0

{- simplePlayer: simplePlayer is a player without any "cleverness" and doesn't use strategies. 
It takes a hand of dominoes, a board state, the current player, and the current scores, 
and returns a domino to play and the side to play it on.-}
    simplePlayer :: DomsPlayer
    simplePlayer hand InitState _ _ = (head hand, L)
    simplePlayer [tile] (State (a,b) (c,d) history) _ _
      | canPlay tile L (State (a,b) (c,d) history) = (tile, L)
      | canPlay tile R (State (a,b) (c,d) history) = (tile, R)
    simplePlayer (hand:rest) (State (a,b) (c,d) history) player (score1,score2)
      | canPlay hand L (State (a,b) (c,d) history) = (hand, L) -- checking for both sides of the domino in hand since we can flip the domino either way
      | canPlay hand R (State (a,b) (c,d) history) = (hand, R)
      | otherwise = simplePlayer rest (State (a,b) (c,d) history) player (score1,score2)


{- canPlay: Given a domino, an end to play it at and a board state 
   ,canPlay checks if the domino can be played at the given end. 
-}
    canPlay :: Domino -> End -> Board -> Bool
    canPlay _ _ InitState = True
    canPlay domino end (State (a,b) (c,d) history) =
      let leftVal = fst domino ; rightVal = snd domino
      in if end == L then a == leftVal || a == rightVal
          else d == leftVal || d == rightVal


{- possPlays: Given the player's hand and the board, possPlays returns a 2 part tuple
   consisting of a list of dominos that can be played at the left end and 
   of a list of dominos that can be played at the right end.
 -}
    possPlays :: Hand -> Board -> ([Domino],[Domino])
    possPlays hand InitState = (hand,hand)
    possPlays (tile:tiles) (State (a,b) (c,d) history) =
      let leftPlays = filter (\tile -> canPlay tile L (State (a,b) (c,d) history)) (tile:tiles)
          rightPlays = filter (\tile -> canPlay tile R (State (a,b) (c,d) history)) (tile:tiles)
      in if not (null leftPlays) && not (null rightPlays) then (leftPlays, rightPlays)
        else if null leftPlays && not (null rightPlays) then ([],rightPlays)
        else if not (null leftPlays) && null rightPlays then (leftPlays, [])
        else ([],[])

{- simulatePlayAndGetScore: Given the current player, a domino that can be played at both ends and the current board, 
   the function simulates playing the tile on both ends and compares the scores gained from 
   playing on left end and on right end. 
   It returns a 3 part tuple consisting of the domino, the end at which playing the tile 
   would produce a higher score and the score produced.
 -}
    simulatePlayAndGetScore :: Player -> Domino  -> Board -> (Domino,End,Int)
    simulatePlayAndGetScore player tile InitState =
      let newBoard = playDom player tile InitState L
          score = case newBoard of
                Just b -> scoreBoard b True
                Nothing -> 0
      in (tile,L,score)
    simulatePlayAndGetScore player tile board =
      let newBoardL = playDom P1 tile board L
          newBoardR = playDom P1 tile board R
          scoreL = case newBoardL of
                  Just b -> scoreBoard b True
                  Nothing -> 0
          scoreR = case newBoardR of
                  Just b -> scoreBoard b True
                  Nothing -> 0
      in if scoreL >= scoreR then (tile, L, scoreL) else (tile, R, scoreR)

-- Helper functions:


{- getFirstTwo: returns the first two elements of a 3 part tuple -}
    getFirstTwo :: (a,b,c) -> (a,b)
    getFirstTwo (a,b,_) = (a,b)

{-  sortScores: Given a list of 3 part tuples consisting of a domino, 
   end and score, sortScores sorts the list based on the score 
   in descending order (highest to lowest). 
-}
    sortScores :: [(Domino, End, Int)] -> [(Domino,End, Int)]
    sortScores [] = []
    sortScores [x] = [x]
    sortScores ((d1,e1,s1):(d2,e2,s2):rest)
        | s1 < s2 = (d2, e2,s2) : sortScores ((d1,e1, s1):rest)
        | otherwise = (d1, e1,s1) : sortScores ((d2,e2, s2):rest)


{- extractBoard: Given a Maybe Board, extractBoard returns just the board. -}
    extractBoard :: Maybe Board -> Board
    extractBoard Nothing = InitState
    extractBoard (Just board) = board

{- smartPlayer:
    smartPlayer is the advanced AI player. It takes a hand of dominoes,
    a board state, the current player, and the current scores, and returns a domino
    to play and the side to play it on.

    It considers 3 different scenarios (i.e. strategies):
    1) If the board is empty, decide the first drop/move. 
    2) If there's only one tile in hand, play that tile on whichever end possible. 
      If it's possible to play on both ends, it uses simulatePlayAndGetScore and 
      plays on the highest scoring end.
    3) It checks the list of tiles playable at the left end and at the right end, 
      checks which among both them produce the highest score, and plays the highest 
      scoring tile.
-}
    smartPlayer :: DomsPlayer
    smartPlayer hand InitState _ _
      | domInHand (5,4) hand = ((5,4),L)
      | domInHand (4,5) hand = ((4,5),L)
      | otherwise = (head hand, L) 
    smartPlayer [tile] board player _
      | canPlay tile L board && canPlay tile R board = let (domino,end, _) = simulatePlayAndGetScore player tile board in (domino,end)
      | canPlay tile L board = (tile, L)
      | canPlay tile R board = (tile,R)
    smartPlayer hand@(tile:tiles) board player (score1, score2) =
      let (leftList, rightList) = possPlays (tile:tiles) board
          score = if player == P1 then score1 else score2
          allPlayableTiles = [(d, e) | d <- leftList, e <- [L]] ++ [(d, e) | d <- rightList, e <- [R]] -- all tiles from leftList and rightList
          scoredTiles = playTheseTiles allPlayableTiles board --list of tiles from both leftlist and rightlist
          sortedScores = sortScores scoredTiles
          highestScoringTile = getFirstTwo (head sortedScores)
      in highestScoringTile
      where
      {- playTheseTiles: Given a list of 2 part tuples consisting of a domino and the end to play it at and the board,
                         it returns a list of 3 part tuples consisting of the domino, end and the score produced from playing the domino.-}
        playTheseTiles [] currentBoard = []
        playTheseTiles ((tile, end):remainingTiles) currentBoard =
          let newBoard = extractBoard (playDom player tile currentBoard end)
              lastTile = null hand
              tileScore = scoreBoard newBoard lastTile
              endTile = end
              scoredTiles = playTheseTiles remainingTiles newBoard
          in ((tile, endTile, tileScore) : scoredTiles)