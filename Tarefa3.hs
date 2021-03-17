{- |

= Introduction of the task:

This task consists of compacting a maze. A compact maze is a way to understand easily the maze, as it becomes
a list with it's elements being tuples preceded by a constructor named "Instruct" if a corridor does not repeat
itself, if it repeats, instead of "Instruct [tuples]", it will show as "Repeat n" with n being the number of
the corridor it repeats. The tuples mentioned are of form (Int,Piece), with the Int being the number of times
the Piece repeats itself consecutively on a corridor.

= Objectives of the task:

1- Verify if a corridor is repeated.
2- Transform corridors into an Instruct.


= How we solved these issues?

1- We converted a corridor into a list of tuples, where the tuples are (1,Piece).

2- We created the samePiece function that receives the list of tuples mentioned before and if there are two 
   tuples with the same piece the function sums both Ints.

3- We created the compactAllMaze function that converts a maze into an Instructions.

4- After doing the three previous points for every corridor, we created the repeatCorridor function that 
   receives the instructions and analyses if there are any Intructs that repeat.

= Discussion and Conclusion

At this task we could have used higher order functions such as foldr or map, but we were not familiar with
this type of programming since at the time it were just starting to learn these functions and we choose to
not use them, instead of not knowing what to do.
We did not enjoy this task as much, as we still do not really understand the purpose of compacting mazes.


-}


module Tarefa3 where
import Tarefa1
import Types

-- * Functions made for testing purposes
-- ** Compacts Mazes with even length and height

-- | Compacts a square Maze with the same even length and height
teste1T3 :: Instructions
teste1T3 = compactMaze (teste1T1)

-- | Compacts a rectangular maze with Maze with more length than height 
teste2T3 :: Instructions
teste2T3 = compactMaze (teste2T1)

-- | Compacts a rectangular maze with Maze with more height than length 
teste3T3 :: Instructions
teste3T3 = compactMaze (teste3T1)

-- ** Compacts Mazes with odd length and height
-- | Compacts a square Maze with the same even length and height
teste4T3 :: Instructions
teste4T3 = compactMaze (teste4T1)

-- | Compacts a rectangular maze with Maze with more length than height 
teste5T3 :: Instructions
teste5T3 = compactMaze (teste5T1)

-- | Compacts a rectangular maze with Maze with more height than length 
teste6T3 :: Instructions
teste6T3 = compactMaze (teste6T1)

-- ** Compacts Mazes with even length and odd height
-- | Compacts a rectangular maze with Maze with more length than height 
teste7T3 :: Instructions
teste7T3 = compactMaze (teste7T1)

-- | Compacts a rectangular maze with Maze with more height than length
teste8T3 :: Instructions
teste8T3 = compactMaze (teste8T1)

-- ** Compacts Mazes with even length and odd height

-- | Generates a rectangular maze with Maze with more length than height
teste9T3 :: Instructions
teste9T3 = compactMaze (teste9T1)

-- | Generates a rectangular maze with Maze with more height than lenght
teste10T3 :: Instructions
teste10T3 = compactMaze (teste10T1)

-- ** Main Function of the task that compacts a Maze
-- | Given a Maze this funtion compacts a Maze
compactMaze :: Maze -> Instructions
compactMaze l = repeatCorridor (compactAllMaze l) 0 

-- *** Auxiliary Functions of compactMaze
-- | Compacts the whole Maze
compactAllMaze :: Maze -> Instructions
compactAllMaze []   = []
compactAllMaze (h:t) = ((Instruct (samePiece (compactCorridor h))) : compactAllMaze t) 

-- | Compacts a Corridor
compactCorridor :: Corridor -> [(Int,Piece)]
compactCorridor []                  = []
compactCorridor (p:t)            = ((1, p) : compactCorridor t)

-- | Evaluates if two pieces are the same and adds them
samePiece :: [(Int,Piece)] -> [(Int,Piece)]
samePiece [(n,p)] = [(n,p)]
samePiece ((n1,p1):(n2,p2):t) | p1 == p2  = samePiece (((n1+n2),p1):t)
                              | otherwise = ((n1,p1) : samePiece ((n2,p2) : t))


-- | Analyzes what instructions repeat themselves
repeatCorridor :: Instructions -> Int -> Instructions
repeatCorridor []  _ = []
repeatCorridor [x] _ = [x]
repeatCorridor [x,y] pos | sameCorridor x y = [x,Repeat pos]
                         | otherwise        = [x,y]
repeatCorridor (x1:x2:xs) pos | (sameCorridor x1 x2) = (x1 : Repeat pos : repeatCorridor ((auxrepeat2 (x1:xs) pos)) (pos+2))
                              | otherwise            = [x1] ++ repeatCorridor (x2:(auxrepeat2 (x1:xs) pos)) (pos+1)   

-- | Auxiliary function of repeatCorridor
auxrepeat2 :: Instructions -> Int -> Instructions
auxrepeat2 []  _ = []
auxrepeat2 [x] _ = [x]
auxrepeat2 [x,y] pos | sameCorridor x y = [Repeat pos]
                     | otherwise        = [y]
auxrepeat2 (y1:y2:ys) pos | (sameCorridor y1 y2) = [Repeat pos] ++ (auxrepeat2 (y1:ys) pos)
                          | otherwise            = [y2] ++ (auxrepeat2 (y1:ys) pos)

-- | Analyzes if two Corridors are the same
sameCorridor :: Instruction -> Instruction -> Bool
sameCorridor (Instruct []) (Instruct []) = True
sameCorridor (Instruct []) _             = False
sameCorridor _ (Instruct [])             = False
sameCorridor _ (Repeat _ )               = False 
sameCorridor (Repeat _) _                = False
sameCorridor (Instruct ((n1,p1):t1)) (Instruct ((n2,p2):t2)) | (n1 == n2 && p1 == p2) = sameCorridor (Instruct t1) (Instruct t2) 
                                                             | otherwise = False


