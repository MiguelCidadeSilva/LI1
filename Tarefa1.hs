{- |

= Introduction of the task:

On this stage of the project we were tasked with creating mazes that would be generated according
to length, height and a seed, with the seed being any Int number.

= Objectives of the task:

A playable maze must have the following properties that became problems to solve:

1- Walls must surround the play area of the maze except a section in the middle which is the tunnel.

2- The tunnel must be alligned with the Ghost house, with it's size depending on the height of the 
   maze.

3- The maze must have a Ghost house on the middle where the Ghosts spawn and go after being eaten by
   a Pacman, similarly to the tunnel, the Ghost house changes it's size according to the length of the
   maze.

= How we solved these issues?

1- Firstly, we know that the first and last lines of the maze are composed by Wall pieces only, so we
   created the generateRoof function that created corridors of a certain length only made by Walls.

2- To create the tunnel and the Ghost house in the correct positions we made the buildHouse functions
   that generate the tunnel and the Ghost house at the same time, according to the length and height
   of the maze.

3- To generate the rest of the maze we created the generateCorridor function that generates corridors
   for the maze that start and finish with Wall pieces with the middle being made by the combination
   of two functions, generateRandoms and convertCorridor, both given by the professors that generate
   a random corridor based on a seed.

The main function (generateMaze) of this task when given the length, the height and the seed (three 
Ints) starts by doing the first point (corridor made by walls) and generates a part of the maze, then
inserts the Ghost house and the tunnel (second point) and generates the rest of the maze inserting a 
last corridor only composed by Walls.


= Discussion and Conclusion

This was the easiest task of the project, and that makes sence since it was our first experience with
Haskell to such depth, and we had never used GitHub before so we were still learning.
But despite that we enjoyed doing this task.

-}


module Tarefa1 where 
import Types
import System.Random

-- * Functions made just for testing

-- ** Test functions that generate Mazes with even lenght and even heigth

-- | Generates a square Maze with the same even length and height
teste1T1 :: Maze
teste1T1  = generateMaze 24 24 536356
teste1T1' = showMaze 24 24 536356

-- | Generates a rectangular maze with Maze with more length than height 
teste2T1 :: Maze
teste2T1 = generateMaze 30 20 327327
teste2T1'= showMaze 30 20 327327

-- | Generates a rectangular maze with Maze with more height than length 
teste3T1 :: Maze
teste3T1  = generateMaze 22 28 747438 
teste3T1' = showMaze 22 28 747438 

-- ** Test functions that generate Mazes with odd length and odd height

-- | Generates a square Maze with the same odd length and height
teste4T1 :: Maze
teste4T1  = generateMaze 27 27 372672 
teste4T1' = showMaze 27 27 372672 

-- | Generates a rectangular maze with Maze with more length than height 
teste5T1 :: Maze
teste5T1  = generateMaze 33 19 432872 
teste5T1' = showMaze 33 19 432872 

-- | Generates a rectangular maze with Maze with more height than length 
teste6T1 :: Maze
teste6T1  = generateMaze 25 33 921821
teste6T1' = showMaze 25 33 921821

-- ** Test functions that generate Mazes with even length and odd height

-- | Generates a rectangular maze with Maze with more length than height 
teste7T1 :: Maze
teste7T1  = generateMaze 34 29 617627
teste7T1' = showMaze 34 29 617627

-- | Generates a rectangular maze with Maze with more height than length
teste8T1 :: Maze
teste8T1  = generateMaze 18 27 732873
teste8T1' = showMaze 18 27 732873

-- ** Test functions that generate Mazes with odd length and even height

-- | Generates a rectangular maze with Maze with more length than height 
teste9T1 :: Maze
teste9T1  = generateMaze 33 22 125612
teste9T1' = showMaze 33 22 125612

-- | Generates a rectangular maze with Maze with more height than lenght
teste10T1 :: Maze
teste10T1  = generateMaze 23 28 621762
teste10T1' = showMaze 23 28 621762

-- | This is an exemple of a Maze
maze = [
       [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
       [Empty, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Empty],
       [Empty, Food Little, Food Little, Food Little, Food Little, Wall, Food Little, Empty],
       [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
       ]

-- * Functions used to generate the Maze

-- ** Functions given by the professors

-- | Given a seed returns a list of n integer randomly generated
--
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                         in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9

-- | Given a seed returns an integer randomly generated
randomNumber :: Int -> Int
randomNumber seed = head $ generateRandoms 1 seed

-- ** Funtions we made

-- ** Funtions made to build the ghost house

-- | Decides the dimension of the ghost house if the x is odd or even 
buildHouse :: Int -> Int -> Int-> [Corridor]
buildHouse x y s| odd x = buildHouseOdd x y s
                | otherwise = buildHouseEven x y s

-- | Builds the corridors of the ghost house including the tunnel if the y (maze height) is odd or even and x (maze length) is odd
buildHouseOdd :: Int-> Int -> Int-> [Corridor]
buildHouseOdd x y s = if (odd y) then [[Wall]  ++ parteCorridorgenerate x s     ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+5) ++ [Wall],
                                       [Wall]  ++ parteCorridorgenerate x (s+1) ++ [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty]     ++ parteCorridorgenerate x (s+6) ++ [Wall],
                                       [Empty] ++ parteCorridorgenerate x (s+2) ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ parteCorridorgenerate x (s+7) ++ [Empty],
                                       [Wall]  ++ parteCorridorgenerate x (s+3) ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty]         ++ parteCorridorgenerate x (s+8) ++ [Wall],
                                       [Wall]  ++ parteCorridorgenerate x (s+4) ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+9) ++ [Wall]]  
-- This else is for when y is even
                      else             [[Wall]  ++ parteCorridorgenerate x s     ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+5) ++ [Wall],
                                        [Wall]  ++ parteCorridorgenerate x (s+1) ++ [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty]     ++ parteCorridorgenerate x (s+6) ++ [Wall],
                                        [Empty] ++ parteCorridorgenerate x (s+2) ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ parteCorridorgenerate x (s+7) ++ [Empty],
                                        [Empty] ++ parteCorridorgenerate x (s+3) ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty]         ++ parteCorridorgenerate x (s+8) ++ [Empty],
                                        [Wall]  ++ parteCorridorgenerate x (s+4) ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+9) ++ [Wall]]        

-- | Builds the corridors of the ghost house house including the tunnel if the y (maze height) is odd or even and x (maze length) is even
buildHouseEven :: Int-> Int -> Int->[Corridor]
buildHouseEven x y s = if (odd y) then [[Wall]  ++ parteCorridorgenerate x s     ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]        ++ parteCorridorgenerate x (s+5) ++ [Wall],
                                        [Wall]  ++ parteCorridorgenerate x (s+1) ++ [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty]     ++ parteCorridorgenerate x (s+6) ++ [Wall],
                                        [Empty] ++ parteCorridorgenerate x (s+2) ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ parteCorridorgenerate x (s+7) ++ [Empty],
                                        [Wall]  ++ parteCorridorgenerate x (s+3) ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty]        ++ parteCorridorgenerate x (s+8) ++ [Wall],
                                        [Wall]  ++ parteCorridorgenerate x (s+4) ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]        ++ parteCorridorgenerate x (s+9) ++ [Wall]]  
-- This else is for when y is even
                      else             [[Wall]  ++ parteCorridorgenerate x s     ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+5) ++ [Wall],
                                        [Wall]  ++ parteCorridorgenerate x (s+1) ++ [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty]      ++ parteCorridorgenerate x (s+6) ++ [Wall],
                                        [Empty] ++ parteCorridorgenerate x (s+2) ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty]  ++ parteCorridorgenerate x (s+7) ++ [Empty],
                                        [Empty] ++ parteCorridorgenerate x (s+3) ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty]         ++ parteCorridorgenerate x (s+8) ++ [Empty],
                                        [Wall]  ++ parteCorridorgenerate x (s+4) ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]         ++ parteCorridorgenerate x (s+9) ++ [Wall]]        
-- *** Auxiliary functions
-- | Generates Corridores that start and finish with a wall piece
generateCorridor :: Int -> Int -> Corridor
generateCorridor 0 seed = []
generateCorridor c seed = [Wall] ++ convertCorridor(generateRandoms (c-2) seed) ++ [Wall]

-- Builds the corridor section between the wall and the ghost house
parteCorridorgenerate :: Int-> Int -> Corridor
parteCorridorgenerate x s = convertCorridor(generateRandoms c s) 
   where
    c = div x 2- 6

-- ** Helpful funtions to convert data types
-- | Convert one integer number into one piece
convertPiece :: Int -> Piece
convertPiece x | x == 3 = Food Big
               | x >= 0 && x <70 = Food Little
               | x >= 70 && x <= 99 = Wall


-- | Convert a list of integers into a corridor
convertCorridor :: [Int] -> Corridor
convertCorridor l = map convertPiece l

-- | Convert a list of lists of integers into a maze
convertMaze :: [[Int]] -> Maze
convertMaze  l = map convertCorridor l

-- ** Funtions used to generate a valid maze

-- | Generates the roof and the floor of the maze
generateRoof :: Int -> Corridor
generateRoof 0 = []
generateRoof c = Wall : generateRoof (c-1)

-- | Main function of "Tarefa1.hs" using the functions generateCorridor and generateRoof this fuction generates a propper maze with walls and no tunnel limiting the play area in list form
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y seed = [generateRoof x] ++
                        (if(even y) then aux1 y1 x seed else aux3 y3 x seed) ++
                        buildHouse x y seed ++
                        (if(even y) then aux2 y2 x (seed+y1) else aux3 y3 x (seed+y3)) ++
                        [generateRoof x]
                where                     
                    y1 = div y 2 - 4
                    y2 = div y 2 - 3
                    y3 = div y 2 - 3

-- *** Auxiliary functions of generateMaze
aux1 :: Int -> Int -> Int -> [Corridor]
aux1 0 _ _ = []
aux1 y1 x seed = generateCorridor x seed : aux1 (y1-1) x (seed +1)

aux2 :: Int -> Int -> Int -> [Corridor]
aux2 0 _ _ = []
aux2 y2 x seed = generateCorridor x seed : aux2 (y2-1) x (seed +1)

aux3 :: Int -> Int -> Int -> [Corridor]
aux3 0 _ _ = []
aux3 y3 x seed = generateCorridor x seed : aux1 (y3-1) x (seed +1)    

-- | Shows a Maze with given 3 integers in IO() form. This function is based on the mazeGenerate function 
showMaze :: Int -> Int -> Int -> IO ()
showMaze x y s = putStrLn (printMaze (generateMaze x y s))
