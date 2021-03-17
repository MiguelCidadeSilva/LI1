{- |

= Introduction of the task:

With maze genaration completed, now we had to do the functions that made the plays that would 
occur durring the game.

= Objectives of the task:

1- The result of a play must move a player one unit in one of the four directions of two axis
(horizontal and vertical), Up, Down, Left and Right, never diagonally.

2- A player can't go through Walls.

3- When a player touches the tunnel, it must be teleported to the other side of the maze.

4- When a player moves to a Empty piece it just moves without changing any of it's attributes.

5- If a Pacman moves to a Little Food, he gains 1 point and makes the move.

6- If a Pacman moves to a Big Food, he gains 5 point, makes the move and changes pacMode to Mega if
   it was Normal with Mega time 10, if Pacman is already on Pacmode Mega before eating the Big Food, he
   just regains Mega time, which goes back to 10 and in both cases Ghosts change their GhostMode to dead.

7- When a Pacman and a Ghost collide, if Pacman is Normal, Pacman looses a life and nothing happens to
   the Ghost, otherwise, when Pacman is Mega, he eats the Ghost, gains 10 points and the Ghost changes
   the GhostMode to Alive and moves to the GhostHouse.

8- When the orientation of the play is different from the orientation of the player the player changes
   only it's orientation and does not move.

= How we solved these issues?

1- When the orientation is different from the the player that has the same ID as the one in the play,
   it only changes orientation.

2- When the ID of the play is the same  as the one in the head of a list of players, and orientation
   of the play is the same as the player and it is Rigth: 

2.1- When the vertical component of the player's coordinates is equal to the (length of the maze-1)
     he moves to the other side with it's vertical component updated to 0.

3- When the ID the play is the same  as the one in the head of a list of players, and orientation of
   the play is the same as the player and it is Left: 

3.1- When the vertical component of the player's coordinates is equal to 0 he moves to the other side 
     with it's vertical component updated to the (length of the maze-1).

4- When the ID of the play is the same  as the one in the head of a list of players, for all orientations:

4.1- For when Pacman moves to a piece where there is a Ghost or when a Ghost moves to piece where there
     is a Pacman, we created the touchGhostResult function that decides the outcome, based on the PacMode.

4.2- For when the Pacman moves to a safe piece (without a Ghost), we created the placeResult function 
     that given the piece and the State with the Pacman at the head of the player list, makes the changes 
     to the Pacman's attributes based on what the piece is.

4.3- For when a Ghost moves to a safe piece (without a Pacman) we created the placeResultGhost function that 
     if the piece is not a Wall the Ghost just changes it's coordinates without changing any other attribute. 

5- When the ID doesn't exist on a player list the main function of the task gives the starting State with
   no changes.

= Discussion and Conclusion

This was the hardest task of the first phase, we had many problems when we were testing it, but we managed
to fix these issues. The most difficult part of this task was translating evertything that was instructed to
us into code.
Still this was a fun experience and probabily our favorite task of the entire first phase. 


-}



module Tarefa2 where
import Tarefa1
import Types

-- * Functions made for testing purposes

-- | Tests a play when a maze has the same length and height
teste1T2 :: State
mazepp = teste3T1
jogadores1 = [(Ghost (GhoState (2,(9,20),1,R,4,2) Dead)),(Pacman (PacState (0,(9,19),1,R,4,2) 0 Open Mega)),(Pacman (PacState (1,(20,12),1,L,10,0) 0 Closed Dying))]
play1 = Move 0 R
state1 = State mazepp jogadores1 1
teste1T2 = play (play1) (state1)

-- | Tests a play when a maze has more length than height  
teste2T2 :: State
mazeii = teste5T1
jogadores2 = [(Ghost (GhoState (2,(14,12),1,R,4,2) Alive)),(Pacman (PacState (0,(13,15),1,L,4,2) 0 Open Normal)),(Pacman (PacState (1,(8,3),1,U,10,0) 0 Closed Normal))]
play2 = Move 1 L
state2 = State mazeii jogadores2 4
teste2T2 = play (play2) (state2)

-- | Tests a play when a maze has more height than length
teste3T2 :: State
mazepi = teste8T1 
jogadores3 = [(Ghost (GhoState (2,(5,7),1,R,4,2) Dead)),(Pacman (PacState (0,(13,4),1,U,4,2) 0 Open Normal)),(Pacman (PacState (1,(4,7),1,D,10,0) 0 Closed Mega))]
play3 = Move 1 D
state3 = State mazepi jogadores3 2
teste3T2 = play (play3) (state3)

-- | Tests a play when a maze has more length than height
teste4T2 :: State
mazeip = teste9T1
jogadores4 = [(Ghost (GhoState (2,(7,14),1,R,4,2) Alive)),(Pacman (PacState (0,(3,5),1,D,4,2) 0 Open Dying)),(Pacman (PacState (1,(12,9),1,R,10,0) 0 Closed Normal)),(Pacman (PacState (2,(2,10),1,R,4,2) 0 Open Normal))]
play4 = Move 2 U
state4 = State mazeip jogadores4 3
teste4T2 = play (play4) (state4)

-- * Functions we created
-- | Main function of the task, this function defines the outcome off all the possible plays
play :: Play -> State -> State
play _ (State maze [] le) = State maze [] le 
play (Move id R)   (State maze (Pacman (PacState (i,(cx,cy),v,R,p,l) t m pm):xs) le) | (id == i && cy == lc)              = State maze (Pacman (PacState (i,(cx,0),v,R,p,l) t m pm):xs) le 
                                                                                     | (id == i && ghostPos (cx,cy+1) xs) = touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,R,p,l) t m pm):xs) le)
                                                                                     | (id == i)                          = placeResult (pieceCoords (cy+2,cx+1) maze) (State maze (Pacman (PacState (i,(cx,cy),v,R,p,l) t m pm):xs) le)                       
                                                                                     | (idExist id xs)                    = play (Move id R) (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,R,p,l) t m pm)]) le)
                                                                                     | otherwise                          = State maze (Pacman (PacState (i,(cx,cy),v,R,p,l) t m pm):xs) le
                                                  where
                                                    lc = lengthCorridor maze - 1

play (Move id L)   (State maze (Pacman (PacState (i,(cx,cy),v,L,p,l) t m pm):xs) le) | (id == i && cy == 0)               = State maze (Pacman (PacState (i,(cx,lengthCorridor maze - 1),v,L,p,l) t m pm):xs) le
                                                                                     | (id == i && ghostPos (cx,cy-1) xs) = touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,L,p,l) t m pm):xs) le)
                                                                                     | (id == i)                          = placeResult  (pieceCoords (cy,cx+1) maze) (State maze (Pacman (PacState (i,(cx,cy),v,L,p,l) t m pm):xs) le)                     
                                                                                     | (idExist id xs)                    = play (Move id L) (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,L,p,l) t m pm)]) le)
                                                                                     | otherwise                          = State maze (Pacman (PacState (i,(cx,cy),v,L,p,l) t m pm):xs) le

play (Move id U)   (State maze (Pacman (PacState (i,(cx,cy),v,U,p,l) t m pm):xs) le) | (id == i && ghostPos (cx-1,cy) xs) = touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,U,p,l) t m pm):xs) le)
                                                                                     | (id == i)                          = placeResult    (pieceCoords (cy+1,cx) maze) (State maze (Pacman (PacState (i,(cx,cy),v,U,p,l) t m pm):xs) le)                      
                                                                                     | (idExist id xs)                    = play (Move id U) (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,U,p,l) t m pm)]) le)
                                                                                     | otherwise                          = State maze (Pacman (PacState (i,(cx,cy),v,U,p,l) t m pm):xs) le

play (Move id D)   (State maze (Pacman (PacState (i,(cx,cy),v,D,p,l) t m pm):xs) le) | (id == i && ghostPos (cx+1,cy) xs) = touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,D,p,l) t m pm):xs) le)
                                                                                     | (id == i)                          = placeResult  (pieceCoords (cy+1,cx+2) maze) (State maze (Pacman (PacState (i,(cx,cy),v,D,p,l) t m pm):xs) le)
                                                                                     | (idExist id xs)                    = play (Move id D) (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,D,p,l) t m pm)]) le)
                                                                                     | otherwise                          = State maze (Pacman (PacState (i,(cx,cy),v,D,p,l) t m pm):xs) le

play (Move id or)  (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le) | (id == i)       = State maze (Pacman (PacState (i,(cx,cy),v,or,p,l) t m pm):xs) le
                                                                                     | (idExist id xs) = play (Move id or) (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm)]) le)
                                                                                     | otherwise       = State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le

play (Move id or)  s@(State maze (Ghost  (GhoState (i,(cx,cy),v,o,p,l) gs):xs) le)   | id == i && cy == 0 && o == or && o == L = State maze (Ghost (GhoState (i,(cx,lengthCorridor maze - 1),v,L,p,l) gs):xs) le  
                                                                                     | id == i && cy == (lengthCorridor maze - 1) && o == or && o == R = State maze (Ghost (GhoState (i,(cx,0),v,R,p,l) gs):xs) le  
                                                                                     | condition1 = touchGhostResult $ headPacman (x,y) s
                                                                                     | condition2 = placeResultGhost piece s
                                                                                     | condition3 = (State maze (Ghost  (GhoState (i,(cx,cy),v,or,p,l) gs):xs) le)
                                                                                     | condition4 = play (Move id or) (State maze (xs ++ [Ghost (GhoState (i,(cx,cy),v,o,p,l) gs)]) le)
                                                                                     | otherwise  = State maze (Ghost (GhoState (i,(cx,cy),v,o,p,l) gs):xs) le 
                                                                             where piece = analyzePiece maze o (cx,cy)
                                                                                   (x,y) = coordsResult (cx,cy) o
                                                                                   condition1 = id == i && checkPacman (cx,cy) s o && or == o
                                                                                   condition2 = id == i && or == o 
                                                                                   condition3 = id == i
                                                                                   condition4 = (idExist id xs)

-- | When given a Maze, this function determines the length of the Corridors that constitute the Maze 
lengthCorridor :: Maze -> Int
lengthCorridor (h:t) = length h

-- | Given coordinates and a list of players, this function verifies if there are any Ghosts that have those coordinates
ghostPos :: Coords -> [Player] -> Bool
ghostPos (_,_) [] = False
ghostPos (x,y) (Ghost  (GhoState (_,(gx,gy),_,_,_,_) _):xs)     | (x == gx && y == gy) = True
                                                                | otherwise            = ghostPos (x,y) xs
ghostPos (x,y) (Pacman (PacState _ _ _ _):xs) = ghostPos (x,y) xs

-- | Verifies if a ID given on the play function exists or not
idExist :: Int -> [Player] -> Bool
idExist id [] = False
idExist id (Pacman (PacState (i,_,_,_,_,_) _ _ _):xs) | id == i   = True
                                                      | otherwise = idExist id xs
idExist id (Ghost (GhoState (i,_,_,_,_,_) _):xs) | id == i   = True
                                                 | otherwise = idExist id xs
-- we did not notice that the guards were not aligned when we pushed this task on the deadline, we corrected it now

-- | Determines the outcome of touching a Ghost in every orientation depending on the Pacstate
touchGhostResult :: State -> State
touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m Mega):xs) le) = case o of
                                                                                                R -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy+1),v,R,p+10+(pieceGhostCoords maze (cy+2,cx+1)),l) t m Mega):(changeGhostToMiddle maze (cx,cy+1) xs))  le
                                                                                                L -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy-1),v,L,p+10+(pieceGhostCoords maze (cy,cx+1))  ,l) t m Mega):(changeGhostToMiddle maze (cx,cy-1) xs))  le
                                                                                                U -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx-1,cy),v,U,p+10+(pieceGhostCoords maze (cy+1,cx))  ,l) t m Mega):(changeGhostToMiddle maze (cx-1,cy) xs))  le
                                                                                                D -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx+1,cy),v,D,p+10+(pieceGhostCoords maze (cy+1,cx+2)),l) t m Mega):(changeGhostToMiddle maze (cx+1,cy) xs))  le

touchGhostResult (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m Normal):xs) le) | (l==1)    = State maze (Pacman (PacState (i,(cx,cy),v,o,p,0) t m Dying):xs) le
                                                                                       | otherwise = restartGame (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l-1) t m Normal):xs) le)
touchGhostResult (State maze (p:ps) le) = touchGhostResult (State maze (ps ++ [p]) le)

-- | Needed to add the points from the foods eaten
pieceGhostCoords :: Maze  -> Coords -> Int
pieceGhostCoords maze (x,y) 
                          | pieceCoords (x,y) maze == (Food Little)  = 1
                          | pieceCoords (x,y) maze == (Food Big) = 5
                          | otherwise = 0

-- | When given a Maze, the coordinates of a Ghost and the list of players, this function sends the ghost to middle of the Maze (Ghost House)
changeGhostToMiddle :: Maze-> Coords -> [Player] -> [Player]
changeGhostToMiddle _ (_,_) [] = []
changeGhostToMiddle maze (gx,gy) (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) = (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm) : (changeGhostToMiddle maze (gx,gy) xs))
changeGhostToMiddle maze (gx,gy) (Ghost  (GhoState (i,(x,y),v,o,p,l) gm):xs)       | (x == gx && y == gy) = (Ghost (GhoState (i,(newCoordsGhost maze),(2*v),o,p,l) Alive): changeGhostToMiddle maze (gx,gy) xs) 
                                                                                   | otherwise            = (Ghost (GhoState (i,(x,y),v,o,p,l) gm):changeGhostToMiddle maze (gx,gy) xs)      
 
-- | Changes the coordinates of the Ghost to the middle of the Maze
newCoordsGhost :: Maze -> Coords
newCoordsGhost maze | (odd a && odd c)  = ((div a 2)    ,(div c 2) - 1)
                    | (odd a && even c) = ((div a 2)    ,(div c 2))
                    | (even a && odd c) = ((div a 2) - 1,(div c 2) - 1)
                    | otherwise         = ((div a 2) - 1,(div c 2))          
            where
              a = length maze
              c = lengthCorridor maze

-- | Auxiliary function of play. Used to determine the results of moving Pacman in any of the four orientations. If pacman eats food he gains points depending on the size of the fruit, if he eats big foods his state changes to Mega and if Pacman doesn't move against a wall he changes his coordinates

placeResult :: Piece -> State -> State
placeResult Empty (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le)         = case o of
                                                                                                       R -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy+1),v,o,p,l) t m pm):xs) le
                                                                                                       L -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy-1),v,o,p,l) t m pm):xs) le
                                                                                                       U -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx-1,cy),v,o,p,l) t m pm):xs) le
                                                                                                       D -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx+1,cy),v,o,p,l) t m pm):xs) le
placeResult (Food Little) (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le) = case o of
                                                                                                       R -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy+1),v,o,p+1,l) t m pm):xs) le
                                                                                                       L -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy-1),v,o,p+1,l) t m pm):xs) le
                                                                                                       U -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx-1,cy),v,o,p+1,l) t m pm):xs) le
                                                                                                       D -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx+1,cy),v,o,p+1,l) t m pm):xs) le

placeResult (Food Big) (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le)    = case o of
                                                                                                       R -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy+1),v,o,p+5,l) 10 m Mega):changeGhostState xs) le
                                                                                                       L -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx,cy-1),v,o,p+5,l) 10 m Mega):changeGhostState xs) le
                                                                                                       U -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx-1,cy),v,o,p+5,l) 10 m Mega):changeGhostState xs) le
                                                                                                       D -> State (pieceToEmpty (cy+1,cx+1) maze) (Pacman (PacState (i,(cx+1,cy),v,o,p+5,l) 10 m Mega):changeGhostState xs) le

placeResult Wall (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le)          = State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le
                                                                                                       

-- | Given Coordinates and a Maze, this function gives back the Piece that was on those coordinates
pieceCoords :: Coords -> Maze -> Piece
pieceCoords (x,y) maze | take y maze == [] = Wall
                       | take x (last (take y maze)) == [] = Wall
                       | otherwise = last (take x (last (take y maze)))

-- | Given a list of players, changes the GhostState
changeGhostState :: [Player] -> [Player]
changeGhostState [] = []
changeGhostState (Ghost  (GhoState (i,(cx,cy),v,o,p,l) gs):xs)      | v == 0.5 = (Ghost (GhoState (i,(cx,cy),v,o,p,l) Dead): changeGhostState xs)
                                                                    | otherwise = (Ghost (GhoState (i,(cx,cy),(v/2),o,p,l) Dead): changeGhostState xs)
changeGhostState (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs)  = (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm) : changeGhostState xs)

-- | Replaces the pieces Pacman eats with Empty on a Maze
pieceToEmpty :: Coords -> Maze -> Maze
pieceToEmpty (_,_) []     = []
pieceToEmpty (x,1) (c:cs) = (corridorPieceToEmpty x c) : cs
pieceToEmpty (x,y) (c:cs) = c : pieceToEmpty (x,y-1) cs

-- | Replaces the pieces Pacman eats with Empty on a Corridor
corridorPieceToEmpty :: Int -> Corridor -> Corridor
corridorPieceToEmpty _ []     = []
corridorPieceToEmpty 1 (p:ps) = Empty:ps
corridorPieceToEmpty x (p:ps) = p:(corridorPieceToEmpty (x-1) ps)

-- ** Functions made after the deadline
-- |  Auxiliary function of play. Used to determine the results of moving a Ghost in any of the four orientations.
placeResultGhost :: Piece -> State -> State
placeResultGhost Wall s = s
placeResultGhost _ (State maze (Ghost  (GhoState (i,(cx,cy),v,o,p,l) gs):xs) le) = State maze (Ghost  (GhoState (i,(gx,gy),v,o,p,l) gs):xs) le
                                                              where
                                                                (gx,gy) = coordsResult (cx,cy) o
                                                                                      

-- | Returns the piece where a player is moving to.
analyzePiece :: Maze -> Orientation -> Coords -> Piece
analyzePiece m o (x,y) = case o of 
                                   R -> pieceCoords (y+2,x+1) m
                                   L -> pieceCoords (y,x+1) m
                                   U -> pieceCoords (y+1,x) m
                                   D -> pieceCoords (y+1,x+2) m

-- | Checks if there is a pacman closed to the ghost given the coordinates of the ghost and the State
checkPacman :: Coords -> State -> Orientation -> Bool
checkPacman (x,y) (State maze lp le) o = case o of 
                                                  R -> pacPos (x,y+1) lp
                                                  L -> pacPos (x,y-1) lp
                                                  U -> pacPos (x-1,y) lp
                                                  D -> pacPos (x+1,y) lp
                                                  Null -> False


-- | Checks if there is a pacman closed to the ghost given the coordinates and the list of players
pacPos :: Coords -> [Player] -> Bool
pacPos (_,_) [] = False
pacPos (x,y) (Pacman (PacState (_,(px,py),_,_,_,_) _ _ _ ):xs) | (x == px && y == py) = True
                                                               | otherwise            = pacPos (x,y) xs
pacPos (x,y) (p:ps) = pacPos (x,y) ps

-- | Puts the Pacman closed to the ghost at the head of the list of players on State
headPacman :: Coords -> State -> State
headPacman (x,y) s@(State maze (Pacman (PacState (i,(px,py),v,o,p,l) t m pm ):xs) le ) | x == px && y == py = s
                                                                                       | otherwise = headPacman (x,y) (State maze (xs ++ [Pacman (PacState (i,(px,py),v,o,p,l) t m pm)]) le)
headPacman (x,y) (State m (p:ps) le) = headPacman (x,y) (State m (ps ++ [p]) le)


-- | Checks the result of the coordinates given the orientation
coordsResult :: Coords -> Orientation -> Coords
coordsResult (x,y) o = case o of 
                                R -> (x,y+1)
                                L -> (x,y-1)
                                U -> (x-1,y)
                                D -> (x+1,y)

-- | When the Pacman looses a life, it and the Ghosts return to their inicial coordinates, Ghosts go to the Ghost house and Pacman goes to the start position below the ghost house
restartGame :: State -> State
restartGame (State m lp le) = State m (map (ghostsMiddle m) lp) le

-- | Changes the coordinates of 1 player when a Pacman touches a Ghost and looses 1 life
ghostsMiddle :: Maze -> Player -> Player
ghostsMiddle m (Ghost  (GhoState (i,(x,y),v,o,p,l) gm)) = Ghost (GhoState (i,(newCoordsGhost m),v,o,p,l) Alive)
ghostsMiddle m (Pacman  (PacState (i,(x,y),v,o,p,l) t mo pm)) = Pacman  (PacState (i,(px,py),v,o,p,l) t mo pm)
     where
      nCG = newCoordsGhost m
      (px,py) = (fst nCG +2, snd nCG)