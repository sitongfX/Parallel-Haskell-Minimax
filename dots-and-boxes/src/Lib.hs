


module Lib
    ( someFunc
    ) where


import qualified Data.Set as Set
import System.IO(Handle, hGetLine, hIsEOF, withFile, IOMode(ReadMode))
import System.Exit(die)
import Control.Monad


someFunc :: String -> IO ()
someFunc config_file = do (eSet, bList) <- readConfig config_file
                          gameStart eSet bList


-- Int = unique identification per edge
-- Bool = whether the edge is taken
data Edge = Edge {eid :: Int, flag :: Bool}


data Box = Box {
    edges :: [Edge],
    val :: Int
} deriving Eq


instance Ord Edge where
  (Edge v1 _) `compare` (Edge v2 _) = v1 `compare` v2


instance Show Edge where
  show (Edge x f) = "(Edge " ++ (show x) ++ " " ++ (show f) ++ ")"
  show _           = "Error: printing Edge obj."

instance Eq Edge where
  (Edge v1 _) == (Edge v2 _) = v1 == v2


instance Show Box where
  show (Box l v) = "Box " ++ (printEdgeList l) ++ " " ++ (show v)
  show _         = "Error: printing Box obj."


printEdgeList :: [Edge] -> String
printEdgeList [] = []
printEdgeList (x:xs) = show x ++ " " ++ printEdgeList xs


readConfig :: String -> IO (Set.Set Edge, [Box])
readConfig fname = withFile fname ReadMode (\h -> initiateGameBoard h)


initiateGameBoard :: Handle -> IO (Set.Set Edge, [Box])
initiateGameBoard h = do res <- hIsEOF h
                         if res
                         then return (Set.empty, [])
                         else do (b, eList) <- initBox h
                                 (eSet, bList) <- initiateGameBoard h
                                 let newESet = foldl (\s e -> Set.insert e s) eSet eList
                                 return (newESet, b : bList)


initBox :: Handle -> IO (Box, [Edge])
initBox h = do line <- hGetLine h
               case (words line) of
                 l@[v, e1, e2, e3, e4] -> do let edgeL = foldl (\acc x -> (Edge (read x) False) : acc) [] (tail l)
                                             let box = Box edgeL (read v)
                                             return (box, edgeL)
                 _ -> die $ "Board Configuration Read Error."


-- note: computer makes the first move
gameStart :: Set.Set Edge -> [Box] -> IO ()
gameStart edgeSet boxList = if (Set.null edgeSet || length boxList == 0)
                               then die $ "Error: starting the game because edge set or box list is empty."
                               else do res <- gameLoop edgeSet boxList False 0
                                       case res `compare` 0 of
                                         LT -> putStrLn "Human WIN!"
                                         EQ -> putStrLn "DRAW!"
                                         GT -> putStrLn "Computer WIN!"
                                         _  -> die "Unexpected result returned from gameLoop."


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
gameLoop :: Set.Set Edge -> [Box] -> Bool -> Int -> IO Int
gameLoop eSet bList t aiScore = if Set.null eSet
                                then return aiScore
                                else if t
                                     then do eId <- getHumanMove eSet
                                             let nextEdgeH =  Edge eId False
                                             let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
                                             gameLoop newEdgeH newBoxH False newScoreH
                                     else do let (_, nextEdgeC) =  minimax False (eSet, bList, aiScore) (Edge 0 False)
                                             let (newEdgeC, newBoxC, newScoreC) = nextGameState nextEdgeC (eSet, bList) aiScore t
                                             gameLoop newEdgeC newBoxC True newScoreC



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
nextGameState e (eSet, bList) score player = if player
                                             then  (newS, newL, score - sUpdate)
                                             else  (newS, newL, score + sUpdate)
                                             where (newS, newL, sUpdate) = gameAction e (eSet, bList)


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
gameAction targetEdge (eSet,bList) = (Set.delete targetEdge eSet, newBoxList, scoreChanged)
  where applyAction (accl, newList) b = if (containEdge targetEdge (edges b)) && (boxFilled b)
                                       then (accl + (val b), newList)
                                       else (accl, (newBox b) : newList)
        newBox box = Box (newEdgeList (edges box)) (val box)
        newEdgeList oldList = (Edge (eid targetEdge) True) : (filter (/=targetEdge) oldList)
        containEdge tar eList = any (==targetEdge) eList
        boxFilled box = all (\(Edge _ f) -> f) (filter (/= targetEdge) (edges box))
        (scoreChanged, newBoxList) = foldl applyAction (0,[]) bList



getHumanMove :: Set.Set Edge -> IO Int
getHumanMove eSet = do putStrLn "Please make the next move."
                       putStrLn $ "Available edges: " ++ (printEdgeList (Set.toList eSet))
                       mv <- getLine
                       return $ read mv



minimax :: Bool -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
minimax player (edgeset,boxlist, aiScore) edge
    | terminal                         = (aiScore, edge)
    | player == False                  = bestMove [minimax True x e | (x, e) <- expandedStates]
    | player == True                   = worstMove [minimax False x e | (x, e) <- expandedStates]
    where
        expandedStates = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]
        terminal = Set.null edgeset



bestMove :: [(Int, Edge)] -> (Int, Edge)
bestMove [(score, edge)]       = (score, edge)
bestMove ((score, edge):(score', edge'):xs) = bestMove (if score >= score' then (score, edge):xs else (score', edge'):xs)
bestMove _  = error "fine"


worstMove :: [(Int, Edge)] -> (Int, Edge)
worstMove [(score, edge)]       = (score, edge)
worstMove ((score, edge):(score', edge'):xs) = worstMove (if score <= score' then (score, edge):xs else (score', edge'):xs)
worstMove _  = error "fine"







-------------------------------------------------------------------------------------------------------


{-
do let edge = head $ filter (\(Edge id _) -> id == eId) (Set.toList eSet)
                                    let newESet = Set.delete edge eSet
                                    let affectedBox = filter checkSameEdge (Set.toList bSet)
                                    let bSet2 = foldl (\b -> Set.delete b bSet) affectedBox
  where 
    checkSameEdge b = any (filter ((Edge id _) -> id == eId) b.edges)
    checkBoxFilled (Box eL v) = all (\(Edge _ f) -> f) eL
-}
