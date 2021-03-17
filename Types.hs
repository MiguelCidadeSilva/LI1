-- | This module contains all sorts of auxiliary types and datas used on the project along with some extra functions and examples of Mazes and a list of Players used in testing
module Types where
import Data.List

-- * Auxiliary data types for "Tarefa1.hs" 
type Maze = [Corridor] -- always horizontal
type Corridor = [Piece]
data Piece = Food FoodType | PacPlayer Player| Wall | Empty deriving (Eq)
data FoodType = Big | Little deriving (Eq)

-- | Shows Pieces as String and attributes a colour
instance Show Piece where
   show (Wall) = coloredString "#" None
   show (Empty) = coloredString " " None
   show (Food z) = coloredString (show z )   Green
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Normal))) = coloredString (show(PacState (i, c, x, y,z,l) o m Normal)) Yellow
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Mega)))   = coloredString (show(PacState (i, c, x, y,z,l) o m Mega))   Blue
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Dying)))  = coloredString (show(PacState (i, c, x, y,z,l) o m Dying))  Red
   show (PacPlayer (Ghost z)) = coloredString (show z)  Purple

-- | Distinguishes food according to size
instance Show FoodType where
   show (Big) =  "o"
   show (Little) =  "."

-- * Auxiliary data types for "Tarefa2.hs" 

data Play = Move Int Orientation deriving (Eq,Show)
data Orientation = L | R | U | D | Null deriving (Eq,Show)
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 
data State = State 
    {
           maze :: Maze
     ,     playersState :: [Player]
     ,     level :: Int
    }
data Player   =  Pacman PacState | Ghost GhoState deriving (Eq)
data PacState =  PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
   
    } deriving Eq
data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode

    } deriving Eq
type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)

-- | Shows States
instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\n Players: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

-- | Shows the different characters used to represent Pacman according PacMode, orientation and if it's mouth is open or closed
instance Show PacState where
     show (PacState (_, (_,_), _, _, _, _) _ _ Dying)  =  "X"
     show (PacState (_, (_,_), _, R, _, _) _ Open _)   = "{"
     show (PacState (_, (_,_), _, L, _, _) _ Open _)   = "}"
     show (PacState (_, (_,_), _, U, _, _) _ Open _)   = "V"
     show (PacState (_, (_,_), _, D, _, _) _ Open _)   = "^" 
     show (PacState (_, (_,_), _, R, _, _) _ Closed _) = "<"
     show (PacState (_, (_,_), _, L, _, _) _ Closed _) = ">"
     show (PacState (_, (_,_), _, U, _, _) _ Closed _) = "v"
     show (PacState (_, (_,_), _, D, _, _) _ Closed _) = "|" 

-- | Shows Player
instance Show Player where
   show (Pacman x) =  show x
   show (Ghost x)  =  show x

-- | Shows ghost according to GhostMode
instance Show GhoState where
   show (GhoState x Dead)  =  "?"
   show (GhoState x Alive) =  "M"


-- * Auxiliary data types for "Tarefa3.hs" 
type Instructions = [Instruction]
data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)

-- * Useful functions for the project, some were not used by us, but we kept them here since they were given by the professors

-- | Converts a Corridor to a string
printCorridor :: Corridor -> String
printCorridor [] =  "\n"
printCorridor (h:t) = show h ++ printCorridor t 


-- | Converts a Maze into a string
printMaze :: Maze -> String
printMaze [] = []
printMaze (h:t) = printCorridor h ++ printMaze t

-- | Convert a list into a list of list of size n
subList :: Int -> [a] -> [[a]]
subList _ [] = []
subList n l = take n l: subList n (drop n l)

{-- | Alternative version of generateMaze that just generates a random invalid Maze using the functions given on "Generator.hs"
generateMaze' :: Int -> Int -> Int -> Maze
generateMaze' l h seed = convertMaze (subList l (generateRandoms (l*h) seed))
--}
-- | Assigns a colour to a string 
coloredString :: String -> Color -> String
coloredString x y = x
{--coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x --}

-- | Given a list of Players and a Maze, puts the Players on the Maze
placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs (replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m)

-- | Prints the Player's stats on the State
printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                         t = getPlayerTime p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++ " Time: " ++ show t ++"\n"

-- 
getPlayerTime :: Player -> Double
getPlayerTime (Pacman (PacState (x,y,z,t,h,l) q c d )) = q
getPlayerTime _ = 0

-- | Given a Player, retrieves it's ID
getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x

-- | Given a Player, retrieves it's points
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

-- | Gives new Coordinates to a Player 
setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )

-- | Given a Piece, retrieves it's Orientation
getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

-- | Given a Pacman, retrieves it's Pacmode
getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

-- | Given a Player, retrieves it's PlayerState
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

-- | Given a Player, retrieves it's Orientation
getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

-- | Given Coordinates, a Piece and a Maze, substitutes the Piece on the Coordinates given in a Maze 
replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

-- | Given an Integer, a Piece and a Corridor, substitutes a certain Piece on a Corridor
replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- | Given a Player, retrieves it's Coordinates 
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- * Examples of mazes and corridors and a list of players to make tests

-- | Example of a list of Players
jsMega :: [Player]
jsMega = [(Ghost (GhoState (2,(9,7),1,R,4,2) Dead)),(Pacman (PacState (0,(6,4),1,L,4,1) 2 Open Mega)),(Ghost (GhoState (1,(3,27),1,D,7,12) Dead))]

jsNormal :: [Player]
jsNormal = [(Ghost (GhoState (2,(9,7),1,R,4,2) Alive)),(Pacman (PacState (0,(6,4),1.5,L,4,1) 0 Open Normal)),(Ghost (GhoState (1,(3,27),1,D,7,12) Alive))]

-- ** These functions only serve to show a generic Mazes and Corridor given  by the teachers

-- | Gives an example of a Maze
mazeExemplo :: Maze 
mazeExemplo = [-- 1    2    3     4    5       6    7      8     9    10    11    12    13    14    15     16   17    18    19    20     21   22    23     24    25    26   27   28     29     30   31
              [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
              [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Empty , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
              [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Little, Food Big,Empty , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
              [Wall, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
              [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Wall, Wall, Empty, Wall, Wall, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty,Empty, Wall],
              [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
              [Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Food Little, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Food Big, Empty, Empty],
              [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Wall, Wall, Wall, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall],
              [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty ,Empty , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
              [Wall, Empty, Empty, Food Little, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Empty , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Little, Empty, Empty, Empty, Empty,Empty, Wall],             
              [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
              ]

-- | Shows the first example of a Maze
mazeExemploIO :: IO ()
mazeExemploIO = putStrLn (printMaze (mazeExemplo))

-- | Gives an example of a Corridor
corridorExemplo :: Corridor
corridorExemplo = [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, (PacPlayer (Pacman (PacState (0,(6,12),1,R,4,2) 0 Open Normal))), Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall]


-- | Gives a second example of a Maze
mazeExemplo2 :: Maze 
mazeExemplo2 = [-- 1    2    3     4    5       6    7      8     9    10    11    12    13    14    15     16   17    18    19    20     21   22    23     24    25    26   27   28     29     30   31
               [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
               [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
               [Wall, Empty, Food Little, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
               [Wall, Empty, Food Little, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
               [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Big, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
               [Wall, Empty, Food Little, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Wall],
               [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
               ]


-- * Tarefa4
-- | Manager data type
data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    }  deriving Show

-- * Test related functions

-- | Gives a third example of a Maze
mazeExemplo3 :: Maze
mazeExemplo3 = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
                [Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall],
                [Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall],
                [Wall,Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty,Wall],
                [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty,Empty],
                [Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall],
                [Wall,Empty,Food Big,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall],
                [Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall],
                [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
               ]



-- | Initial state for the main game
sT1 :: State
sT1 = State mazeExemplo3 jsTeste2 1

-- | Initial player list for the main game
jsTeste :: [Player]
jsTeste = [(Pacman (PacState (0,(9,12),1,R,0,2) 0 Open Normal)),(Ghost (GhoState (1,(7,12),1,U,0,0) Alive)),(Ghost (GhoState (2,(7,12),1,U,0,0) Alive)),(Ghost (GhoState (3,(7,12),1,U,0,0) Alive)),(Ghost (GhoState (4,(7,12),1,U,0,0) Alive))]

-- | Alternative player list for the main game
jsTeste2 :: [Player]
jsTeste2 = [(Pacman (PacState (0,(6,5),1,R,0,2) 0 Open Normal)),(Ghost (GhoState (1,(4,5),1,U,0,0) Alive)),(Ghost (GhoState (2,(4,5),1,U,0,0) Alive)),(Ghost (GhoState (3,(4,5),1,U,0,0) Alive)),(Ghost (GhoState (4,(4,5),1,U,0,0) Alive))]

-- | Given a string converts it to a corridor
mazeCorridor :: String -> Corridor
mazeCorridor [] = []
mazeCorridor c = foldr (\x acc -> sToPiece x : acc) [] c


-- | Converts a char into a piece
sToPiece :: Char -> Piece
sToPiece '#' = Wall
sToPiece ' ' = Empty
sToPiece '.' = Food Little
sToPiece 'o' = Food Big

-- | Maze of the second level represented as a list of strings
mazeTesteS2 :: [String]
mazeTesteS2 = ["#########################"
              ,"#o......#.......#......o#"
              ,"#.##.#..#.#...#.#..#.##.#"
              ,"#...o#....#...#....#o...#"
              ,"#.####..#.#...#.#..####.#"
              ,"#......           ......#"
              ,"##...## ###   ### ##...##"
              ,"....... #       # ...... "
              ,"##.##.# ######### #.##.##"
              ,"#...#.#           #.#...#"
              ,"#.#.#.#...#####...#.#.#.#"
              ,"#.#o..###.......###..o#.#"
              ,"#.###.#.....#.....#.###.#"
              ,"#o..........#..........o#"
              ,"#########################"
              ]

-- | Maze of the second level
mazeTeste2 :: Maze
mazeTeste2 = map mazeCorridor mazeTesteS2

-- | State of the second level
sT2 :: State
sT2 = State mazeTeste2 jsTeste 2

-- | Maze of the third level represented as a list of strings
mazeTesteS3 :: [String]
mazeTesteS3 = ["#########################"
              ,"#o...#.............#...o#"
              ,"#.##....####.####....##.#"
              ,"#.#..#...o##.##o...#..#.#"
              ,"#.#.##..####.####..##.#.#"
              ,"#......           ......#"
              ,"##...## ###   ### ##...##"
              ,"....... #       # ...... "
              ,"##.##.# ######### #.##.##"
              ,"#...#.#           #.#...#"
              ,"#.#.#.....##.##.....#.#.#"
              ,"#.#o#.#.##.....##.#.#o#.#"
              ,"#.#.#.#....#o#....#.#.#.#"
              ,"#o....#############....o#"
              ,"#########################"
              ]

-- | Maze of the third level
mazeTeste3 :: Maze
mazeTeste3 = map mazeCorridor mazeTesteS3

-- | State of the third level
sT3 :: State
sT3 = State mazeTeste3 jsTeste 3


-- | Maze of the fourth level represented as a list of strings
mazeTesteS4 :: [String]
mazeTesteS4 = ["#########################"
              ,"#o..........#.#...#....o#"
              ,"###.#######...##..###.###"
              ,"#......#....#...........#"
              ,"#.#.#..#..#####.#######.#"
              ,"#o#.#..           ###...#"
              ,"###.### ###   ### ..#..##"
              ,"....... #       # ..#... "
              ,"###.### ######### ..#..##"
              ,"#......           ###.###"
              ,"#.#...#.##.##.#....#..#o#"
              ,"#.#...#.#...#.#.#.##..#.#"
              ,"#.#.###.##.##.#.#..#..#.#"
              ,"#....o#....o....#.......#"
              ,"#########################"
              ]

-- | Maze of the fourth level
mazeTeste4 :: Maze
mazeTeste4 = map mazeCorridor mazeTesteS4

-- | State of the fourth level
sT4 :: State
sT4 = State mazeTeste4 jsTeste 4

-- | Maze we tried to implement on Gloss as a list of strings
mazeGlossSt :: [String]
mazeGlossSt     = ["#########################"
                  ,"#.......#.......#.......#"
                  ,"#.###.#.#.#.#.#.#.#.###.#"
                  ,"#.....#...#.#.#...#.....#"
                  ,"#.###.###.#.#.#.###.###.#"
                  ,"#.#.......#.#.#.......#.#"
                  ,"#.#.###.....#.....###.#.#"
                  ,"#........#######........#"
                  ,"#.###.#           #.###.#"
                  ,"#.###.# ###   ### #.###.#"
                  ," .....# #       # #..... "
                  ," .###.# ######### #.###. "
                  ,"#...#.#           #.#...#"
                  ,"###.#.#..#######..#.#.###"
                  ,"#...#.......#.......#...#"
                  ,"#.###.###...#...###.###.#"
                  ,"#.....#...........#.....#"
                  ,"#.#.###.#.#####.#.###.#.#"
                  ,"#.#.....#.......#.....#.#"
                  ,"#.#####.#########.#####.#"
                  ,"#.......................#"
                  ,"#########################"
                  ]

-- | Maze we tried to implement on Gloss
mazeGloss :: Maze
mazeGloss = map mazeCorridor mazeGlossSt

-- | Player list we tried to implement on Gloss
jsGloss :: [Player] 
jsGloss = [(Pacman (PacState (0,(16,12),1,R,0,0) 0 Open Normal)),(Ghost (GhoState (1,(10,12),1,U,0,0) Alive)),(Ghost (GhoState (2,(10,12),1,U,0,0) Alive)),(Ghost (GhoState (3,(10,12),1,U,0,0) Alive)),(Ghost (GhoState (4,(10,12),1,U,0,0) Alive))]


