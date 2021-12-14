import qualified Data.Set as Set
import System.IO(Handle, hGetLine, hIsEOF, withFile, IOMode(ReadMode))
import System.Exit(die)
import Control.Monad

{-
module Lib
    ( someFunc
    ) where
-}

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Int = unique identification per edge
-- Bool = whether the edge is taken
data Edge = Edge Int Bool deriving Eq


data Box = Box { 
    edges :: [Edge],
    val :: Int
} deriving Eq

{-
instance Ord Edge where
  (Edge v1 b1) `compare` (Edge v2 b2) | v1 > b2   = GT
                                      | v2 > v1   = LT
                                      | otherwise = b1 `compare` b2
-}
instance Ord Edge where
  (Edge v1 _) `compare` (Edge v2 _) = v1 `compare` v2


instance Show Edge where
  show (Edge x f) = "(Edge " ++ (show x) ++ " " ++ (show f) ++ ")"
  show _           = "Error: printing Edge obj."


instance Show Box where
  show (Box l v) = "Box " ++ (printEdgeList l) ++ " " ++ (show v)
  show _         = "Error: printing Box obj."


printEdgeList :: [Edge] -> String
printEdgeList [] = []
printEdgeList (x:xs) = show x ++ " " ++ printEdgeList xs


readConfig :: String -> Handle
readConfig fname = withFile fname ReadMode (\h -> initiateGameBoard h)


-- initiateGameBoard :: Handle -> IO ()
initiateGameBoard :: (Set.Set Edge, Set.Set Box)
initiateGameBoard h = do res <- hIsEOF h 
                         if res
                         then (Set.empty, Set.empty)
                         else do (b, eList) <- initBox h
                                 let (eSet, bSet) = initiateGameBoard h
                                 let newESet = foldl (\s e -> Set.insert e s) eSet eList
                                 return (newESet, Set.insert b bSet)


-- initBox :: Handle -> IO ()
initBox :: Handle -> IO (Box, [Edge])
initBox h = do line <- hGetLine h
               case (words line) of 
                 l@[v, e1, e2, e3, e4] -> do let edgeL = foldl (\acc x -> (Edge (read x) False) : acc) [] (tail l)
                                             let box = Box edgeL (read v)
                                             return (box, edgeL)
                 _ -> die $ "Board Configuration Read Error."    


-- note: computer makes the first move
gameStart :: Set.Set Edge -> Set.Set Box -> IO ()
gameStart edgeSet boxSet = if (Set.empty edgeSet || Set.empty boxSet)
                               then die $ "Error: starting the game because edge set or box set is empty."
                               else do let res = gameLoop edgeSet boxSet False 0 0 
                                       case res of
                                         1  -> putStrLn "Human WIN!"
                                         0  -> putStrLn "DRAW!"
                                         -1 -> putStrLn "Computer WIN!"
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
gameLoop :: Set.Set Edge -> Set.Set Map -> Bool -> Int -> Int -> Int
gameLoop eSet bList t cPlayer hPlayer = if Set.empty eSet
                                        then cmpScore cPlayer hPlayer
                                        else if t  
                                             then do let mv =  getHumanMove eSet    -- TODO: 
                                             else -- TODO: logic for computer move --> do block
    


{-
cmpScore:

param1: computer score
param2: human score

Return: -1 = COMPUTER win 
         0 = Tie
         1 = HUMAN win

-}
cmpScore :: Int a => a -> a -> a
cmpScore cScore hScore | cScore > hScore = -1
                       | hScore > cScore =  1
                       | otherwise       =  0


-- TODO: give an edge id, remove it from edge set and remove box from box list, return new set and box
-- TODO: write a Ord instance for Box (needed by Set)2
gameAction :: Int -> Set.Set Edge -> Set.Set Box -> Int -> IO (Set.Set Edge, Set.Set Box, Int)
gameAction eId eSet bSet score = result
  where 

do let edge = head $ filter (\(Edge id _) -> id == eId) (Set.toList eSet)
                                    let newESet = Set.delete edge eSet
                                    let affectedBox = filter checkSameEdge (Set.toList bSet)
                                    let bSet2 = foldl (\b -> Set.delete b bSet) affectedBox

                               

  where 
    checkSameEdge b = any (filter ((Edge id _) -> id == eId) b.edges)
    checkBoxFilled (Box eL v) = all (\(Edge _ f) -> f) eL
                                   


getHumanMove :: eSet -> IO Int
getHumanMove eSet = do putStrLn "Please make the next move."
                       putStrLn $ "Available edges: " ++ (printEdgeList (Set.toList eSet))
                       mv <- getLine
                       return $ read mv
                    
















{-

class Game: #A class for managing different situations and states happening in the game and on the board

    def Initiate(self): #initiating the game board with X and Y dimensions
        for i in range(0, self.dimY):
            R = []
            for j in range (0, self.dimX):
                if i % 2 == 1 and j % 2 == 1:
                    R.append(randint(1, 9))  # Assigning a random number from 1 to 9 to the spots in the board as the points
                elif i % 2 == 0 and j % 2 == 0:
                    R.append('*') # printing asterisks as the dots in the board
                else:
                    R.append(' ') # adding extra space for actions in the game
            self.Mat.append(R)

    def action(self, i, j): # Applying the actions made by the human or the computer
        Sum = 0

        if j % 2 == 0 and i % 2 == 1:
            self.Mat[j][i] = '-'
            if j < self.dimY - 1:
                if self.Mat[j+2][i] == '-' and self.Mat[j+1][i+1] == '|' and self.Mat[j+1][i-1] == '|':
                    Sum += self.Mat[j+1][i]
            if j > 0:
                if self.Mat[j-2][i] == '-' and self.Mat[j-1][i+1] == '|' and self.Mat[j-1][i-1] == '|':
                    Sum += self.Mat[j-1][i]

        else:
            self.Mat[j][i] = '|'
            if i < self.dimX - 1:
                if self.Mat[j][i+2] == '|' and self.Mat[j+1][i+1] == '-' and self.Mat[j-1][i+1] == '-':
                    Sum += self.Mat[j][i+1]
            if i > 0:
                if self.Mat[j][i-2] == '|' and self.Mat[j+1][i-1] == '-' and self.Mat[j-1][i-1] == '-':
                    Sum += self.Mat[j][i-1]
        return Sum


class Algo: # A class for defining algorithms used (minimax and alpha-beta pruning)
    
    def miniMax(State, Ply_num): # Function for the minimax algorithm

        for i in range(State.Current.dimY):
            for j in range(State.Current.dimX):
                if State.Current.Mat[i][j] == ' ' and (j, i) not in State.children:
                    State.Make(j, i, True)
                    if Ply_num < 2:
                        return (i, j)

        Minimum_Score = 1000
        i = 0
        j = 0
        for k, z in State.children.items():
            Result = Algo.Maximum(z, Ply_num - 1, Minimum_Score)
            if Minimum_Score > Result:
                Minimum_Score = Result
                i = k[0]
                j = k[1]

        return (i, j)


    def Maximum(State, Ply_num, Alpha): # Alpha-beta pruning function for taking care of Alpha values
        if Ply_num == 0:
            return State.CurrentScore

        for i in range(State.Current.dimY):
            for j in range(State.Current.dimX):
                if State.Current.Mat[i][j] == ' ' and (j, i) not in State.children:
                    State.Make(j, i, False)

        Maximum_Score = -1000
        i = 0
        j = 0
        for k, z in State.children.items():
            Result = Algo.Minimum(z, Ply_num - 1, Maximum_Score)
            if Maximum_Score < Result:
                Maximum_Score = Result
            if Result > Alpha:
                return Result

        return Maximum_Score


    def Minimum(State, Ply_num, Beta): # Alpha-beta pruning function for taking care of Beta values
        if Ply_num == 0:
            return State.CurrentScore

        for i in range(State.Current.dimY):
            for j in range(State.Current.dimX):
                if State.Current.Mat[i][j] == ' ' and (j, i) not in State.children:
                    State.Make(j, i, True)

        Minimum_Score = 1000
        i = 0
        j = 0
        for k, z in State.children.items():
            Result = Algo.Maximum(z, Ply_num - 1, Minimum_Score)
            if Minimum_Score > Result:
                Minimum_Score = Result
            if Result < Beta:
                return Result

        return Minimum_Score


class DotsNBoxes: # A class for managing the moves made by the human and the computer
    def __init__(self, Board_Xdim, Board_Ydim, Ply_num):
        currentState = Game([], Board_Xdim, Board_Ydim)
        currentState.Initiate()
        self.State = Thing(currentState)
        self.Ply_num = Ply_num
        self.Score = 0

    def Human(self): # Defining the Human player and his actions/Choices
        self.State.Draw()

        HumanX = int(input("Please enter the 'X' coordinate of your choice (an integer such as 4): "))
        HumanY = int(input("Please enter the 'Y' coordinate of your choice (an integer such as 4): "))
        if (HumanX, HumanY) not in self.State.children:
            self.State.Make(HumanX, HumanY, False)
            self.State = self.State.children[(HumanX, HumanY)]
        else:
            self.State = self.State.children[(HumanX, HumanY)]

        print("Current Score =====>> Your Score - AI Score = " + str(self.State.CurrentScore),end ="\n\n\n")

        self.Computer()


    def Computer(self): # Defining the Computer player and its actions/Choices
        self.State.Draw()

        move = Algo.miniMax(self.State, self.Ply_num)

        self.State = self.State.children[(move[0], move[1])]

        print("AI selected the following coordinates to play:\n" + "(" ,str(move[0]), ", " + str(move[1]), ")", end = "\n\n")

        print("Current Score =====>> Your Score - AI Score = " + str(self.State.CurrentScore), end = "\n\n\n")

        if len(self.State.children) == 0:
            self.State.Draw()
            self.Evaluation()
            return

        self.Human()

    def Evaluation(self): # Evaluation function for taking care of the final scores
        print("Stop this Madness!!!\n")
        if self.State.CurrentScore > 0:
            print("You won you crazy little unicorn!! You are the new hope for the mankind!")
            exit()
        elif self.State.CurrentScore < 0:
            print("!!! Inevitable Doom!!! You were crushed by the AI!! ")
            exit()
        else:
            print("Draw! Well Congratulations! you are as smart as the AI!")
            exit()

    def start(self):
        self.Human()

-}