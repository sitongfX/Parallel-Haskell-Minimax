module Lib
  ( gameFunc,
  )
where

import Control.Parallel.Strategies (parList, rseq, using)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace ()
import Foreign.C.String (castCharToCSChar)
import System.Exit (die)
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

gameFunc :: String -> String -> IO ()
gameFunc config_file mode = do
  (eSet, bList) <- readConfig config_file
  case mode of
    "algo-seq" -> do
      putStrLn "Running SEQUENTIAL minimax algorithm with tree depth = 9"
      let (score, edge) = minimaxDepLim False 9 (eSet, bList, 0) (Edge 0 False)
      putStrLn $ "algorithm suggested: " ++ show edge
      return ()
    "algo-par" -> do
      let pDepth = 1
      putStrLn $ "Running PARALLEL minimax algorithm with tree depth = 9 and parallel depth = " ++ show pDepth
      let (score, edge) = minimaxParDep (False, pDepth, 9, (eSet, bList, 0), Edge 0 False)
      putStrLn $ "algorithm suggested: " ++ show edge
      return ()
    "game-seq" -> do
      putStrLn "Playing game with SEQUENTIAL minimax algorithm"
      depth <- getAlgoDepth
      putStrLn "Game Board: "
      mapM_ print bList
      putStrLn "Available edges :"
      putStrLn $ "" ++ printEdgeList (Set.toList eSet)
      putStrLn "SEQUENTIAL Game is starting..."
      gameStartSeq eSet bList depth
    "game-par" -> do
      putStrLn "Playing game with PARALLEL minimax algorithm"
      putStrLn "Game Board: "
      mapM_ print bList
      putStrLn "Available edges :"
      putStrLn $ "" ++ printEdgeList (Set.toList eSet)
      depth <- getAlgoDepth
      pDepth <- getParDepth
      putStrLn (printEdgeList (Set.toList eSet))
      putStrLn "PARALLEL Game is starting..."
      gameStartPar eSet bList depth pDepth
    _ -> die "Invalid game mode: options are 'algo-seq', 'algo-par', 'game-seq', 'game-par'"

-- Int = unique identification per edge
-- Bool = whether the edge is taken
data Edge = Edge {eid :: Int, flag :: Bool}

data Box = Box
  { edges :: [Edge],
    val :: Int
  }
  deriving (Eq)

instance Ord Edge where
  (Edge v1 _) `compare` (Edge v2 _) = v1 `compare` v2

instance Show Edge where
  show (Edge x f) = "(Edge " ++ show x ++ " " ++ show f ++ ")"

instance Eq Edge where
  (Edge v1 _) == (Edge v2 _) = v1 == v2

instance Show Box where
  show (Box l v) = "Box " ++ printEdgeList l ++ " " ++ show v

printEdgeList :: [Edge] -> String
printEdgeList [] = []
printEdgeList (x : xs) = show x ++ " " ++ printEdgeList xs

readConfig :: String -> IO (Set.Set Edge, [Box])
readConfig fname = withFile fname ReadMode initiateGameBoard

initiateGameBoard :: Handle -> IO (Set.Set Edge, [Box])
initiateGameBoard h = do
  res <- hIsEOF h
  if res
    then return (Set.empty, [])
    else do
      (b, eList) <- initBox h
      (eSet, bList) <- initiateGameBoard h
      let newESet = foldl (flip Set.insert) eSet eList
      return (newESet, b : bList)

initBox :: Handle -> IO (Box, [Edge])
initBox h = do
  line <- hGetLine h
  case words line of
    l@[v, e1, e2, e3, e4] -> do
      let edgeL = foldl (\acc x -> Edge (read x) False : acc) [] (tail l)
      let box = Box edgeL (read v)
      return (box, edgeL)
    _ -> die "Board Configuration Read Error."

getAlgoDepth :: IO Int
getAlgoDepth = do
  putStrLn "Enter a depth for the AI search tree: "
  putStrLn "(if running on 2x2 board, enter depth < 9 for speed up): "
  read <$> getLine

getParDepth :: IO Int
getParDepth = do
  putStrLn "Enter a depth for parallelism: "
  read <$> getLine


-- note: computer makes the first move
gameStartSeq :: Set.Set Edge -> [Box] -> Int -> IO ()
gameStartSeq edgeSet boxList depth =
  if Set.null edgeSet || null boxList
    then die "Error: starting the game because edge set or box list is empty."
    else do
      -- putStrLn "inside game starts, before game loop"
      res <- gameLoopSeq edgeSet boxList False 0 depth
      case res `compare` 0 of
        LT -> putStrLn "Human WIN!"
        EQ -> putStrLn "DRAW!"
        GT -> putStrLn "Computer WIN!"

-- note: computer makes the first move
gameStartPar :: Set.Set Edge -> [Box] -> Int -> Int -> IO ()
gameStartPar edgeSet boxList depth pDepth =
  if Set.null edgeSet || null boxList
    then die "Error: starting the game because edge set or box list is empty."
    else do
      -- putStrLn "inside game starts, before game loop"
      res <- gameLoopPar edgeSet boxList False 0 depth pDepth
      case res `compare` 0 of
        LT -> putStrLn "Human WIN!"
        EQ -> putStrLn "DRAW!"
        GT -> putStrLn "Computer WIN!"

{-
gameLoop: send game control between player and computer

param 1: set of remaining edges
param 2: list of remaining boxes
param 3: flag denoting turn (computer = False, human = True)
param 4: integer cumulative score of computer
param 5: integer cumulative score of human

Return: -1 = COMPUTER win
         0 = Tie
         1 = HUMAN win

-}
gameLoopSeq :: Set.Set Edge -> [Box] -> Bool -> Int -> Int -> IO Int
gameLoopSeq eSet bList t aiScore depth
  | Set.null eSet = return aiScore
  | t = do
    putStrLn "HUMAN move:"
    putStrLn "Available box list: "
    mapM_ print bList
    eId <- getHumanMove eSet
    let nextEdgeH = Edge eId False
    let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
    putStrLn $ "Score after human move: " ++ show newScoreH
    putStrLn "-----------------------------------------------"
    gameLoopSeq newEdgeH newBoxH False newScoreH depth
  | otherwise = do
    putStrLn "AI move:"
    let (_, nextEdgeC) = minimaxDepLim False depth (eSet, bList, aiScore) (Edge 0 False)
    putStrLn $ "AI chose:" ++ show nextEdgeC
    let (newEdgeC, newBoxC, newScoreC) = nextGameState nextEdgeC (eSet, bList) aiScore t
    putStrLn $ "Score after AI move: " ++ show newScoreC
    putStrLn "-----------------------------------------------"
    gameLoopSeq newEdgeC newBoxC True newScoreC depth

gameLoopPar :: Set.Set Edge -> [Box] -> Bool -> Int -> Int -> Int -> IO Int
gameLoopPar eSet bList t aiScore depth pDepth
  | Set.null eSet = return aiScore
  | t = do
    putStrLn "HUMAN move:"
    putStrLn "Available box list: "
    mapM_ print bList
    eId <- getHumanMove eSet
    let nextEdgeH = Edge eId False
    let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
    putStrLn $ "Score after human move: " ++ show newScoreH
    putStrLn "-----------------------------------------------"
    gameLoopSeq newEdgeH newBoxH False newScoreH depth
  | otherwise = do
    putStrLn "AI move:"
    let (_, nextEdgeC) = minimaxParDep (False, pDepth, depth, (eSet, bList, aiScore), Edge 0 False)
    putStrLn $ "AI chose:" ++ show nextEdgeC
    let (newEdgeC, newBoxC, newScoreC) = nextGameState nextEdgeC (eSet, bList) aiScore t
    putStrLn $ "Score after AI move: " ++ show newScoreC
    putStrLn "-----------------------------------------------"
    gameLoopSeq newEdgeC newBoxC True newScoreC depth

{-
cmpScore:

param1: computer score
param2: human score

Return: -1 = COMPUTER win
         0 = Tie
         1 = HUMAN win

cmpScore :: Int a => a -> a -> a
cmpScore cScore hScore | cScore > hScore = -1
                       | hScore > cScore =  1
                       | otherwise       =  0
-}

nextGameState :: Edge -> (Set.Set Edge, [Box]) -> Int -> Bool -> (Set.Set Edge, [Box], Int)
nextGameState e (eSet, bList) score player =
  if player
    then (newS, newL, score - sUpdate)
    else (newS, newL, score + sUpdate)
  where
    (newS, newL, sUpdate) = gameAction e (eSet, bList)

{-
gameAction:

param1: player selected edge (type of Edge) to remove
param2: (x1, x2, x3), s.t. x1 is the current set of available edges,
                           x2 is the current list of available boxes,
                           x3 is the current score of the player.

Return: (e1, e2, e3), s.t. e1 is new set of remaining edges,
                           e2 is new list of remaining boxes,
                           e3 is the updated score
-}

gameAction :: Edge -> (Set.Set Edge, [Box]) -> (Set.Set Edge, [Box], Int)
gameAction targetEdge (eSet, bList) = (Set.delete targetEdge eSet, newBoxList, scoreChanged)
  where
    applyAction (accl, newList) b
      | not (containEdge targetEdge (edges b)) = (accl, b : newList)
      | containEdge targetEdge (edges b) && boxFilled b = (accl + val b, newList)
      | otherwise = (accl, newBox b : newList)
    newBox box = Box (newEdgeList (edges box)) (val box)
    newEdgeList oldList = Edge (eid targetEdge) True : filter (/= targetEdge) oldList
    containEdge tar eList = targetEdge `elem` eList
    boxFilled box = all (\(Edge _ f) -> f) (filter (/= targetEdge) (edges box))
    (scoreChanged, newBoxList) = foldl applyAction (0, []) bList

getHumanMove :: Set.Set Edge -> IO Int
getHumanMove eSet = do
  putStrLn "Please make the next move."
  putStrLn $ "Available edges: " ++ show (printEdgeList $ Set.toList eSet)
  read <$> getLine


-- base minimax
-- minimax :: Bool -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
-- minimax player (edgeset, boxlist, aiScore) edge
--   | rootnode        = bestMove [minimax True x e | (x, e) <- initExpandedStates]
--   | terminal        = (aiScore, edge)
--   | not player      = bestMove [minimax True x e | (x, e) <- subExpandedStates]
--   | player          = worstMove [minimax False x e | (x, e) <- subExpandedStates]
--   | otherwise       = error "invalid game state"
--   where
--     initExpandedStates = [(getNextGameState e, e) | e <- edgelist]
--     subExpandedStates = [(getNextGameState e, edge) | e <- edgelist]
--     getNextGameState someEdge = nextGameState someEdge (edgeset, boxlist) aiScore player
--     edgelist = Set.toList edgeset
--     terminal = Set.null edgeset
--     rootnode = edge == Edge 0 False


{-
minimaxDepLim:

param1: player -> current player (True for human, False for AI) 
param2: depth -> levels remained to traverse from current node 
param3: (edgeset, boxlist, aiScore) ->  edgeset is a set of available edges,
                                        boxlist is a list of available boxes,
                                        aiScore is the current score of AI/board.
param4: edge -> the initially chosen edge for this branch of the tree
return: (aiScore, Edge) -> best move for AI
-}

minimaxDepLim :: (Eq t, Num t) => Bool -> t -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
minimaxDepLim player depth (edgeset, boxlist, aiScore) edge
  | rootnode     = bestMove [minimaxDepLim True newDepth x e | (x, e) <- initExpandedStates]
  | terminal || depth == 0 = (aiScore, edge)
  | not player   = bestMove [minimaxDepLim True newDepth x e | (x, e) <- subExpandedStates]
  | player       = worstMove [minimaxDepLim False newDepth x e | (x, e) <- subExpandedStates]
  | otherwise    = error "invalid game state"
  where
    newDepth = depth - 1
    initExpandedStates = [(getNextGameState e, e) | e <- edgelist]
    subExpandedStates = [(getNextGameState e, edge) | e <- edgelist]
    getNextGameState someEdge = nextGameState someEdge (edgeset, boxlist) aiScore player
    edgelist = Set.toList edgeset
    terminal = Set.null edgeset
    rootnode = edge == Edge 0 False

{-
minimaxParDep:

param1: player -> current player (True for human, False for AI) 
param2: parDepth -> parallel levels remained to traverse from current node
param2: seqDepth -> sequential levels remained to traverse from current node 
param4: (edgeset, boxlist, aiScore) ->  edgeset is a set of available edges,
                                        boxlist is a list of available boxes,
                                        aiScore is the current score of AI/board.
param5: edge -> the initially chosen edge for this branch of the tree
return: (aiScore, Edge) -> best move for AI
-}
minimaxParDep :: (Eq a, Eq t, Num t, Num a) => (Bool, a, t, (Set.Set Edge, [Box], Int), Edge) -> (Int, Edge)
minimaxParDep (player, parDepth, seqDepth, (edgeset, boxlist, aiScore), edge)
  | parDepth == 0            = minimaxDepLim player seqDepth (edgeset, boxlist, aiScore) edge
  | rootnode                 = bestMove parResultInitMax
  | terminal                 = (aiScore, edge)
  | not player               = bestMove parResultSubMax
  | player                   = worstMove parResultSubMin
  | otherwise                = error "invalid game state"
  where
    parResultInitMax = map minimaxParDep paramListInitMax `using` parList rseq
    parResultSubMax = map minimaxParDep paramListSubMax `using` parList rseq
    parResultSubMin = map minimaxParDep paramListSubMax `using` parList rseq

    newParDepth = parDepth - 1
    newSeqDepth = seqDepth - 1
    paramListInitMax = [(True, newParDepth, newSeqDepth, x, e) | (x, e) <- initExpandedStates]
    paramListSubMax = [(True, newParDepth, newSeqDepth, x, e) | (x, e) <- subExpandedStates]
    paramListSubMin = [(False, newParDepth, newSeqDepth, x, e) | (x, e) <- subExpandedStates]

    initExpandedStates = [(getNextGameState e, e) | e <- edgelist]
    subExpandedStates = [(getNextGameState e, edge) | e <- edgelist]
    getNextGameState someEdge = nextGameState someEdge (edgeset, boxlist) aiScore player
    edgelist = Set.toList edgeset
    terminal = Set.null edgeset
    rootnode = edge == Edge 0 False

{-
bestMove:

param1: [(score, edge)] -> a list of available move and the associated heuristic

return: (aiScore, Edge) -> best move for AI(with highest heuristic)
-}
bestMove :: [(Int, Edge)] -> (Int, Edge)
bestMove [(score, edge)] = (score, edge)
bestMove ((score, edge) : (score', edge') : xs) = bestMove (if score >= score' then (score, edge) : xs else (score', edge') : xs)
bestMove _ = error "BestMove: not a valid move"

{-
worstMove:

param1: [(score, edge)] -> a list of available move and the associated heuristic

return: (aiScore, Edge) -> worst move for AI(with lowerst heuristic)
-}
worstMove :: [(Int, Edge)] -> (Int, Edge)
worstMove [(score, edge)] = (score, edge)
worstMove ((score, edge) : (score', edge') : xs) = worstMove (if score <= score' then (score, edge) : xs else (score', edge') : xs)
worstMove _ = error "WorstMove: not a valid move"
