


module Lib
    ( someFunc
    ) where


import qualified Data.Set as Set
import System.IO(Handle, hGetLine, hIsEOF, withFile, IOMode(ReadMode))
import System.Exit(die)
import Control.Monad
import Debug.Trace
import qualified Data.Map.Strict as Map
import Foreign.C.String (castCharToCSChar)

-- import Control.Parallel.Strategies


someFunc :: String -> IO ()
someFunc config_file = do (eSet, bList) <- readConfig config_file
                        --   depth <- getDepth
                          putStrLn (printEdgeList (Set.toList eSet))
                        --   writeFile "testfile.txt" $ show "minimax result"
                          putStrLn "before game starts"
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

getDepth :: IO Int
getDepth = do putStrLn "Enter a depth: "
              depth <- getLine
              return (read depth)


-- note: computer makes the first move
gameStart :: Set.Set Edge -> [Box] -> IO ()
gameStart edgeSet boxList = if (Set.null edgeSet || length boxList == 0)
                               then die $ "Error: starting the game because edge set or box list is empty."
                               else do putStrLn "inside game starts, before game loop"
                                       res <- gameLoop edgeSet boxList False 0
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
                                     then do putStrLn "before human move"
                                             eId <- getHumanMove eSet
                                             let nextEdgeH =  Edge eId False
                                             let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
                                             putStrLn "finish human start computer"
                                             putStrLn $ "score" ++ (show newScoreH)
                                             resH <- gameLoop newEdgeH newBoxH False newScoreH
                                             return resH
                                     else do putStrLn "AI move"
                                             let (_, nextEdgeC) =  minimax False (eSet, bList, aiScore) (Edge 0 False)
                                             let (newEdgeC, newBoxC, newScoreC) = nextGameState nextEdgeC (eSet, bList) aiScore t
                                             putStrLn $ "score" ++ (show newScoreC)
                                             res <- gameLoop newEdgeC newBoxC True newScoreC
                                             return res



-- gameLoop :: Set.Set Edge -> [Box] -> Bool -> Int -> IO Int
-- gameLoop eSet bList t aiScore = if Set.null eSet
--                                 then return aiScore
                                -- else if t
                                --      then do putStrLn "human move"
                                --              putStrLn "print boxlist"
                                --              mapM_ print bList
                                --              eId <- getHumanMove eSet
                                --              let nextEdgeH =  Edge eId False
                                --              let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
                                --              putStrLn $ "score" ++ (show newScoreH)
                                --              resH <- gameLoop newEdgeH newBoxH False newScoreH
                                --              return resH
                                --      else do putStrLn "computer move"
                                --              putStrLn "print boxlist"
                                --              mapM_ print bList
                                --              eId <- getHumanMove eSet
                                --              let nextEdgeH =  Edge eId False
                                --              let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
                                --              putStrLn $ "score" ++ (show newScoreH)
                                --              resH <- gameLoop newEdgeH newBoxH True newScoreH
                                --              return resH







-- gameLoop :: Set.Set Edge -> [Box] -> Bool -> Int -> IO Int
-- gameLoop eSet bList t aiScore = if Set.null eSet
--                                 then return aiScore
--                                 else if t
--                                      then do putStrLn "before human move ,eid"
--                                              eId <- getHumanMove eSet
--                                              putStrLn "in game move after human move"
--                                              let nextEdgeH =  Edge eId False
--                                              let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (eSet, bList) aiScore t
--                                              putStrLn "finish human start computer"
--                                              resH <- gameLoop newEdgeH newBoxH False newScoreH
--                                              return resH
--                                      else do putStrLn "in game move before computer move"
--                                              putStrLn (printEdgeList (Set.toList eSet))
--                                              putStrLn "minimax"
--                                              let (bestscore, nextEdgeC) =  minimax False (eSet, bList, aiScore) (Edge 0 False)
--                                         --      putStrLn (show nextEdgeC)
--                                         --      putStrLn (show bestscore)

--                                             --  let (_, nextEdgeC) =  minimax False (eSet, bList, aiScore) (Edge 0 False)
--                                             --  putStrLn (show nextEdgeC)
--                                         --      putStrLn "finished minimax"
--                                         --      putStrLn (printEdgeList (Set.toList eSet))
--                                             --  let newe = (Edge 1 False) 
--                                             --  let newE = Set.delete newe eSet
--                                             --  putStrLn "New Edge Set:"
--                                             --  putStrLn (printEdgeList (Set.toList newE))
--                                              -- putStrLn (show $ Set.member  eSet)
--                                         --      let (newEdgeC, newBoxC, newScoreC) = nextGameState (Edge 1 False) (eSet, bList) aiScore t
--                                         --      putStrLn "after nextGameState for computer"
--                                         --      putStrLn (printEdgeList (Set.toList newEdgeC))
--                                         --      putStrLn "finish computer and start human"
--                                             --  res <- gameLoop newEdgeC newBoxC True newScoreC
--                                         --      putStrLn "before human move ,eid"
--                                         --      eId <- getHumanMove newEdgeC
--                                         --      putStrLn "in game move after human move"
--                                         --      let nextEdgeH =  Edge eId False
--                                         --      let (newEdgeH, newBoxH, newScoreH) = nextGameState nextEdgeH (newEdgeC, newBoxC) newScoreC True
--                                         --      putStrLn "finish human start computer"
--                                              return bestscore
                                            --  resH <- gameLoop newEdgeH newBoxH False newScoreH
                                            --  return resH


{-
computerPlay :: Set.Set Edge -> [Box] -> Bool -> Int -> IO Int
computerPlay eSet bList aiScore = if Set.null eSet
                                  then return aiScore
                                  else do putStrLn "before computer move"
                                          let (_, nextEdgeC) =  minimax False (eSet, bList, aiScore) (Edge 0 False)
                                          let (newEdgeC, newBoxC, newScoreC) = nextGameState nextEdgeC (eSet, bList) aiScore t
                                          putStrLn "finish computer and start human"
                                          res <- gameLoop newEdgeC newBoxC newScoreC
                                          return res
-}

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
  where applyAction (accl, newList) b = if not (containEdge targetEdge (edges b))
                                       then (accl, b:newList)
                                       else if (containEdge targetEdge (edges b)) && (boxFilled b)
                                            then (accl + (val b), newList)
                                            else (accl, (newBox b) : newList)
        newBox box = Box (newEdgeList (edges box)) (val box)
        newEdgeList oldList = (Edge (eid targetEdge) True) : (filter (/=targetEdge) oldList)
        containEdge tar eList = any (==targetEdge) eList
        boxFilled box = all (\(Edge _ f) -> f) (filter (/= targetEdge) (edges box))
        (sx, er) = foldl applyAction (0,[]) bList
        (scoreChanged, newBoxList) = (trace (show sx) sx, er)
        -- newEdgeSet = Set.delete targetEdge eSet

{-
gameAction :: Edge -> Set.Set Edge-> Set.Set Edge
gameAction targetEdge (eSet,bList) = (Set.delete targetEdge eSet, newBoxList, scoreChanged)

        -- newEdgeSet = Set.delete targetEdge eSet
-}

getHumanMove :: Set.Set Edge -> IO Int
getHumanMove eSet = do putStrLn "Please make the next move."
                       putStrLn $ "Available edges: " ++ (show (printEdgeList $ Set.toList eSet))
                       mv <- getLine
                       return $ read mv

------------------------------------------------------------------------------------------------





-- minimax player (edgeset,boxlist, aiScore) edge
--     | terminal                 = (3, edge)
--     | player == False                          =  bestMove [minimax True x e | (x, e) <- (head expandedStates):[]]
--     | player == True                           =  worstMove [minimax False x e | (x, e) <- (head expandedStates):[]]
--     where
--         es = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]

-- minimax :: Bool -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
-- minimax player depth (edgeset,boxlist, aiScore) edge
--     | terminal || depth == 0                   = (aiScore, edge)
--     | player == False                          =  bestMove [minimax True newdepth x e | (x, e) <- expandedStates]
--     | player == True                           =  worstMove [minimax False newdepth x e | (x, e) <-  expandedStates]
--     where
--         es = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]

--         expandedStates = trace (show es) es
--         ed = depth - 1
--         newdepth = trace ((show ed)) ed
--         terminal = Set.null edgeset

-- appendText x = appendFile "testfile.txt" x




-- cached version

-- terminal            -> reached leaf node
-- player == False     -> ai's turn or max node
-- player == True      -> human's turn or min node

-- list[[value,edge],[edgeset,edgeset,edgeset]]


-- minimax player (edgeset, boxlist, aiScore) edge cacheMap
--     | terminal                                 =  (aiScore, edge)
--     | player == False                          =  if cached then cachedMove else MaxMove
--     | player == True                           =  if cached then cachedMove else MinMove
--     where
--         expandedStates = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]
--         MaxMove = bestMove [minimax True x e cacheMap | (x, e) <- expandedStates]          (bestMove)
--         MinMove = worstMove [minimax False x e maxCacheMap | (x, e) <-  expandedStates]  ??????  先check 有没有edgeset 没的话就call MINIMAX xxx 然后再加上edgeset？？cachmap
--         maxCacheMap = Map.insert edgeset MaxMove cacheMap                                ？？ 但给哪里传Maxcachemap这个parameter
--         minCasheMap = Map.insert edgeset MinMove cacheMap                        
--         cached = Map.member edgeset cacheMap
--         cachedMove = getCachedState edgeset cacheMap
--         terminal = Set.null edgeset


-- use if edgeset exists in cacheMap
-- getCachedState edgeset cacheMap = do
--      move <- Map.lookup edgeset cacheMap
--      return move


-- depth-limited version

-- minimax player depth (edgeset,boxlist, aiScore) edge
--     | terminal                     = (aiScore, edge)
--     | player == False                          = bestMove [minimax True (depth-1) x e | (x, e) <- expandedStates]
--     | player == True                           = worstMove [minimax False (depth-1) x e | (x, e) <- expandedStates]
--     where
--         es = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]

--         -- ((newedgeLsit,_,_), ne) = head expandedStates
--         expandedStates = trace (show es) es

--         -- newdepth = trace (show ed) ed
--         terminal = Set.null edgeset




-- original version

-- minimax :: Bool -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
-- minimax player (edgeset,boxlist, aiScore) edge
--     | edge ==  (Edge 0 False)    = bestMove showbs1
--     | terminal                    = (aiScore, edge)
--     | not player                  = bestMove [minimax True x e | (x, e) <- expandedStates2]
--     | player                      = worstMove [minimax False x e | (x, e) <- expandedStates2]
--     where
--         expandedStates  = [ (nextGameState e (edgeset, boxlist) aiScore player, e) | e <- Set.toList edgeset]
--         expandedStates2 = [ (nextGameState e (edgeset, boxlist) aiScore player, edge) | e <- Set.toList edgeset]
--         -- bs = [minimax True x e | (x, e) <- expandedStates2]
--         bs1 = [minimax True x e | (x, e) <- expandedStates] 
--         showbs1 = trace ("---------" ++show bs1) bs1
--         -- showbs = trace ("---------/n" ++show bs) bs
--         terminal = Set.null edgeset



minimax :: Bool -> (Set.Set Edge, [Box], Int) -> Edge -> (Int, Edge)
minimax player (edgeset,boxlist, aiScore) edge
    | edge == Edge 0 False       = bestMove [minimax True x e | (x, e) <- initExpandedStates]
    | terminal                   = (aiScore, edge)
    | not player                 = bestMove [minimax True x e | (x, e) <- subExpandedStates]
    | player                     = worstMove [minimax False x e | (x, e) <- subExpandedStates]
    | otherwise                  = error "invalid game state"
    where
        initExpandedStates = [ (getNextGameState e, e) | e <- edgelist]
        subExpandedStates = [ (getNextGameState e, edge) | e <- edgelist]
        getNextGameState someEdge = nextGameState someEdge (edgeset, boxlist) aiScore player
        edgelist = Set.toList edgeset
        terminal = Set.null edgeset


bestMove :: [(Int, Edge)] -> (Int, Edge)
bestMove [(score, edge)]       = (score, edge)
bestMove ((score, edge):(score', edge'):xs) = bestMove (if score >= score' then (score, edge):xs else (score', edge'):xs)
bestMove _  = error "not a valid move"

worstMove :: [(Int, Edge)] -> (Int, Edge)
worstMove [(score, edge)]       = (score, edge)
worstMove ((score, edge):(score', edge'):xs) = worstMove (if score <= score' then (score, edge):xs else (score', edge'):xs)
worstMove _  = error "not a valid move"







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
