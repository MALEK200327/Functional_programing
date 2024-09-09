{-  
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

   You should add your functions and any additional types that you require to your own copy of
   this file. Before you submit, make sure you update this header documentation to remove these
   instructions and replace them with authorship details and a brief summary of the file contents.

   Similarly, remember you will be assessed not *just* on correctness, but also code style,
   including (but not limited to) sensible naming, good functional decomposition, good layout,
   and good comments.
 -}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)
    import Data.Bool (Bool(False))
    import Text.ParserCombinators.ReadPrec (reset)
    import Data.Foldable (any, all)
    import Data.List (head)



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
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (elem domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (elem domino hand2) = Nothing
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


    {- scoreBoard: given the current board state and Bool (of whether it is the last domion or not), returns an Int which is the score
        for the current board 
     -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState _ = 0
    scoreBoard (State (x, y) (a,b) _) bool
        | not bool = case () of -- if false calculate the current score of the board according to 3 and 5 rules 
          () | (x == y) && (a==b) && ((x+y+a+b) `mod` 5 == 0) && ((x+y+a+b) `mod` 3 == 0) -> ((x + a+b + y) `div` 5) + ((x+b+a+y) `div` 3)
             | (x == y) && (a==b) && ((x+y+a+b) `mod` 5 == 0) -> (x + a+b + y) `div` 5
             | (x == y) && (a==b) && ((x+y+a+b) `mod` 3 == 0) -> (x + a+b + y) `div` 3
             | (x == y) && ((x+y+b) `mod` 5 == 0) && ((x+y+b) `mod` 3 == 0) -> ((x+b+y) `div` 5) + ((x+b+y) `div` 3)
             | (x == y) && ((x+y+b) `mod` 5 == 0) -> (x+b+y) `div` 5
             | (x == y) && ((x+y+b) `mod` 3 == 0) -> (x+b+y) `div` 3
             | (a == b) && (((x+a+b) `mod` 5 == 0) && ((x+a+b) `mod` 3 == 0)) -> ((x+b+a) `div` 5) + ((x+b+a) `div` 3)
             | (a == b) && ((x+a+b) `mod` 5 == 0) -> (x+b+a) `div` 5
             | (a == b) && ((x+a+b) `mod` 3 == 0) -> (x+b+a) `div` 3
             | (x+b) `mod` 3 == 0 && (a/=b) && (x/=y) -> (x + b) `div` 3
             | (x+b) `mod` 5 == 0 && (a/=b) && (x/=y) -> (x + b) `div` 5
             | otherwise -> 0
        | otherwise = case () of --if true then +1 to the current score of the board
          () | (x == y) && (a==b) && ((x+y+a+b) `mod` 5 == 0) && ((x+y+a+b) `mod` 3 == 0) -> ((x + a+b + y) `div` 5) + ((x+b+a+y) `div` 3) + 1
             | (x == y) && (a==b) && ((x+y+a+b) `mod` 5 == 0) -> (x + a+b + y) `div` 5 + 1
             | (x == y) && (a==b) && ((x+y+a+b) `mod` 3 == 0) -> (x + a+b + y) `div` 3 + 1
             | (x == y) && ((x+y+b) `mod` 5 == 0) && ((x+y+b) `mod` 3 == 0) -> ((x+b+y) `div` 5) + ((x+b+y) `div` 3) + 1
             | (x == y) && ((x+y+b) `mod` 5 == 0) -> (x+b+y) `div` 5 + 1
             | (x == y) && ((x+y+b) `mod` 3 == 0) -> (x+b+y) `div` 3 + 1
             | (a == b) && ((x+a+b) `mod` 5 == 0) && ((x+a+b) `mod` 3 == 0) -> ((x+b+a) `div` 5) + ((x+b+a) `div` 3) + 1
             | (a == b) && ((x+a+b) `mod` 5 == 0) -> (x+b+a) `div` 5 + 1
             | (a == b) && ((x+a+b) `mod` 3 == 0) -> (x+b+a) `div` 3 + 1
             | (x+b) `mod` 3 == 0 && (a/=b) && (x/=y) -> (x + b) `div` 3 + 1
             | (x+b) `mod` 5 == 0 && (a/=b) && (x/=y) -> (x + b) `div` 5 + 1
             | otherwise -> 1

    {-blocked: takes hand and board checks if any domino in the hand can be played on the board then return true otherwise false
    -}
    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False
    blocked [] _ = True
    blocked hand (State (x, _) (_, b) _) =
      all (\(q, w) -> q /= x && q /= b && w /= x && w /= b) hand


    {- playDom: given a player, a domino, a board and an end, returns a new board if the move is valid, otherwise returns Nothing.
      updating the histroy each time as well-}

    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player (q,w) InitState end = Just $ State (q,w) (q,w) [((q,w), player,1)]
    playDom player (q,w) (State (x,y) (a,b) history) end
      | end == L && q == x = Just $ State (w,q) (a,b) (((q,w), player, length history + 1) : history)
      | end == L && w == x = Just $ State (q,w) (a,b) (((q,w), player, length history + 1) : history)
      | end == R && q == b = Just $ State (x,y) (q,w) (((q,w), player, length history + 1) : history)
      | end == R && w == b = Just $ State (x,y) (w,q) (((q,w), player, length history + 1) : history)
      | otherwise = Nothing




    {- simplePlayer: given a hand, a board, a player and scores, returns the first move from simplePlayerMoves function -}
    simplePlayer :: DomsPlayer
    simplePlayer hand board player scores = head  (simplePlayerMoves hand board)

    {-simplePlayerMoves: given a hand and a board then output a list of all the possible moves for the simple player.
    if it is InitState it just output the first tile in the hand -}
    simplePlayerMoves :: Hand -> Board -> [(Domino, End)]
    simplePlayerMoves hand InitState = [(head hand,L)]
    simplePlayerMoves hand (State (x, _) (_, b) _) =
      filter canPlay1 [(d, L) | d <- hand] ++ filter canPlay2 [(d, R) | d <- hand]
        where
         canPlay1 ((q, w), L) = (q == x) || (w == x)
         canPlay2 ((q, w), R) = (q == b) || (w == b)


    {-smartPlayer:given hand,boar,player and scores then choose the best strategy to play the best tile.
    apply this strategy If you have the majority of one particular spot value (e.g. four 6s) it’s a good idea to play that value -} 
    smartPlayer :: DomsPlayer
    smartPlayer hand board player scores =
      case mostRepeated (smartPlayerMoves hand board) of
        (mostRepeatedNumber, count)
         | count > 3 -> playMostRepeatedTile hand board player mostRepeatedNumber
         | otherwise -> highestScore hand board player


{-smartPlayerMoves:  given a hand and a board then output a list of all the possible moves for the simple player.
 applying the following strategy :If you have ‘first drop’ onto an empty board, a popular choice is (5,4), because it scores 3 but the
maximum you can score in reply is 2-}
    smartPlayerMoves :: Hand -> Board -> [(Domino, End)]
    smartPlayerMoves hand InitState
      | (5, 4) `elem` hand = [((5, 4),L)]
      | (6, 3) `elem` hand = [((6, 3),L)]
      | (6, 6) `elem` hand = [((6, 6),L)]
      | otherwise = [(head hand,L)]
    smartPlayerMoves hand (State (x, _) (_, b) _) =
      filter canPlay1 [(d, L) | d <- hand] ++ filter canPlay2 [(d, R) | d <- hand]
        where
          canPlay1 ((q, w), L) = (q == x) || (w == x)
          canPlay2 ((q, w), R) = (q == b) || (w == b)

    {-applying strategy of choosing the tile that score the highest points from smartPlayerMoves then output this tile-}
    highestScore :: Hand -> Board -> Player -> (Domino, End)
    highestScore hand board player =
      fst $ maximumBy (comparing snd) possibleMovesWithScores
      where
        possibleMovesList = smartPlayerMoves hand board
        possibleMovesWithScores = map (\(domino, end) -> ((domino, end), scoreMove domino end)) possibleMovesList
        scoreMove domino end = case playDom player domino board end of
          Just newBoard -> scoreBoard newBoard (blocked hand newBoard)
          Nothing -> 0



{-playMostRepeatedTile: choose 1 tile to play (based on the tile scoring the highest point) from the list of the repeated numbers of tiles -}
    playMostRepeatedTile :: Hand -> Board -> Player -> Int -> (Domino, End)
    playMostRepeatedTile hand board player repeatedNumber =
      highestScore (map fst matchingDominos) board player
      where
        matchingDominos = filter (\(domino, _) -> hasRepeatedNumber domino) (smartPlayerMoves hand board)
        hasRepeatedNumber (x, y) = x == repeatedNumber || y == repeatedNumber


{-mostRepeated: gets a tuple of the most repeated number from a given tiles and how many times it is repeated-}
    mostRepeated :: [(Domino, End)] -> (Int, Int)
    mostRepeated dominosWithEnd =
      let
        flattened = concatMap (\((x, y), _) -> if x == y then [x] else [x, y]) dominosWithEnd
        grouped = group (sort flattened)
        maxGroup = maximumBy (comparing length) grouped
        in (head maxGroup, length maxGroup)




