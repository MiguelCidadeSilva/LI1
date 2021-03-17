{- |

= Introduction of the Task

In this task we have to make plays for every Ghost on a player list, so that the Ghosts move on the game,
based on the current State.
This in a way makes it so that we created AI for the Ghosts, with that in mind there were many possibilities
to consider to try and make the most intelingent Ghosts possible, which ended up making our game a bit harder
than the original one, but the AI could be easily tweaked to become a bit less challenging to a player, making
the Ghosts make bad plays in certain cases allowing Pacman to move with more freedom.

= Objectives of the task:


1- Decide what will be the best play to separate two or more Ghosts on the same position, so they don't
   overlap during their movement.

2- Decide what will be the best play if there is a Pacman and Ghost, both close to the tunnel.

3- Decide the best play for a Ghost when it is at the Ghost House.

4- Decide what will be the best play to do based on a Ghost's GhostMode and the Pacmans PacMode.

= How we solved these issues?

1- We started by creating a function, sameCoords, that analyzes the player list and checks if the Ghost at 
   the head of the player list of a given state, is in the same position as another Ghost. If this happens
   and the Ghost is not close to the Ghost House, the Ghost that is on the head of the player list will make
   a play given by the function called differentPlay, a function that prioritizes the vertical play.

2- When the Ghost at the head of the player list is close to the tunnel and there is a Pacman also close to the 
   tunnel:

2.1- If the Ghost is Alive, the play will be determined by the function playTunnelChase given the maze, the 
     ID of the Ghost, the tail of the player list, the coordinates of the Ghost and the Pacman close to the 
     tunnel and the pieces to the right and the left of the Ghost.

2.2- If the Ghost is Dead the play will be determined by the function playTunnelScatter given the maze,
     the ID of the Ghost, the coordinates of the Ghost and the Pacman closest to the tunnel the pieces to 
     the right and the left of the Ghost.

3- When the Ghost is in the Ghost house the best play will be given by the function playHouse that when given
   the piece one vertical unit higher than the Ghost's coordinates, the difference between the horizontal
   and vertical components of the Ghost and the closest Pacman to it, the ID of the Ghost, the coordinates of
   the Ghost and the list of coordinates of the Ghost house.

4- If the conditions present on the previous points were not verified the play will be given by the function
   chaseMode that recieves the current State of the game and the ID of a Ghost.

4.1- The function chaseMode will call another function named bestPlayChase that when given the coordinates
     of the Ghost, the ID of the Ghost, a list of players without the Ghost we want to move and the maze.  
     On this function, bestPlayChase, the play prioritizes horizontal plays rather than vertical ones, but
     if a part of the column where the closest Pacman is, with that part being located between the the height
     of the Player and the height the Pacman, is only composed by walls then the function will make a vertical 
     play. 


5- If the conditions present on the previous points were not verified the play will be given by the function
   scatterMode that recieves the current State of the game and the ID of a Ghost.
 
5.1- The function scatterMode will call another function named bestPlayScatter that when given the coordinates
    of the Ghost, the ID of the Ghost, a list of players without the Ghost we want to move and the maze.  

6- If the head of the player list is a Pacman and there are Ghosts at the tail of the list, this Pacman will
   move to the end of the list.

7- When there is not any Ghost on the player list, the main function of the task retrieves an empty list.

= Discussion and Conclusion

This was the most difficult task of the project for us since it was one of the most important stages of the
project, and it was our first experience with artificial inteligence.
As the time passed, we found new cases to improve our AI, so we kept adding new things until the end of our
work.
Sometimes, our AI is not as good we want, but it is only in specific cases, so in general we are pretty
proud and satisfied with we accomplished.
This and Tarefa6 were the tasks that more enjoyable to code.

-}


module Tarefa5 where 
import Tarefa2
import Types

teste1T5 :: [Play]
teste1T5 = ghostPlay (State mazeTeste2 jsTeste1T5 2)

jsTeste1T5 :: [Player]
jsTeste1T5 = [(Pacman (PacState (0,(13,11),1,D,0,2) 0 Open Normal)),(Ghost (GhoState (1,(9,12),1,U,0,0) Alive)),(Ghost (GhoState (2,(7,2),1,U,0,0) Alive)),(Ghost (GhoState (3,(7,22),1,U,0,0) Alive)),(Ghost (GhoState (4,(10,8),1,U,0,0) Alive))]

teste2T5 :: [Play]
teste2T5 = ghostPlay (State mazeTeste2 jsTeste2T5 2)

jsTeste2T5 :: [Player]
jsTeste2T5 = [(Pacman (PacState (0,(13,11),1,L,0,2) 0 Open Mega)),(Ghost (GhoState (1,(9,12),1,U,0,0) Dead)),(Ghost (GhoState (2,(7,2),1,U,0,0) Dead)),(Ghost (GhoState (3,(7,22),1,U,0,0) Dead)),(Ghost (GhoState (4,(10,8),1,U,0,0) Dead))]

teste3T5 :: [Play]
teste3T5 = ghostPlay (State mazeTeste2 jsTeste3T5 2)

jsTeste3T5 :: [Player]
jsTeste3T5 = [(Pacman (PacState (0,(1,3),1,R,0,2) 0 Open Normal)),(Ghost (GhoState (1,(9,12),1,U,0,0) Alive)),(Ghost (GhoState (2,(7,2),1,U,0,0) Alive)),(Ghost (GhoState (3,(6,20),1,U,0,0) Alive)),(Ghost (GhoState (4,(3,22),1,U,0,0) Alive))]

-- | Main Function of Tarefa5: given a State makes a list of Plays for every ghost deppending on their Ghosts Mode
ghostPlay :: State -> [Play]
ghostPlay (State m [] l) = []
ghostPlay (State m lp@(Ghost (GhoState pL@(i,(gx,gy),v,o,p,l) gm):xs) le) | sameCoords lp && (tx,ty) == (0,0) = differentPlay (gx,gy) i lp m gm : ghostPlay (State m xs le)
                                                                          | (tx,ty) /= (0,0) && closeTunnel m (gx,gy) && gm == Alive = (playTunnelChase m i xs (gx,gy) (tx,ty) pr pl) : ghostPlay (State m xs le) 
                                                                          | (tx,ty) /= (0,0) && closeTunnel m (gx,gy) && gm == Dead = (playTunnelScatter m i (gx,gy) (tx,ty) pr pl) : ghostPlay (State m xs le) 
                                                                          | elem (gx,gy) lc = (playHouse pu dp dv dvp i (gx,gy) lc) : ghostPlay (State m xs le)
                                                                          | gm == Alive = (chaseMode (State m (Ghost (GhoState pL Alive):xs) le) i)  : ghostPlay (State m xs le)
                                                                          | otherwise   = (scatterMode (State m (Ghost (GhoState pL Dead):xs) le) i) : ghostPlay (State m xs le)
                                            where
                                              lc = houseCoords m
                                              pu = pieceCoords (gy+1,gx)   m  -- Up Piece 
                                              dv = gy - (snd (head lc)) 
                                              (cx,cy) = closestPacman xs (gx,gy) (px,py)
                                              (px,py) = firstPacman xs 
                                              dp = gy - cy
                                              dvp = gx - cx
                                              pl = pieceCoords (gy,gx+1)   m -- Left Piece
                                              pr = pieceCoords (gy+2,gx+1) m -- Right Piece 
                                              (tx,ty) = getCloseTunnelP m xs                                                                                         
ghostPlay (State m (p:ps) le) | verifyPlayerList ps = []
                              | otherwise = ghostPlay (State m (ps ++ [p]) le)

-- | Analyzes if at least two Ghosts on the same list of players have the same coordinates
sameCoords :: [Player] -> Bool
sameCoords [] = False
sameCoords l = elem (head lm) (tail lm)
  where
    lm = map getCoordsPlayer l

-- | Auxiliary function of sameCoords used to get the coordinates of the player
getCoordsPlayer :: Player -> Coords
getCoordsPlayer (Pacman (PacState (_,y,_,_,_,_) _ _ _)) = (0,0)
getCoordsPlayer (Ghost (GhoState (_,y,_,_,_,_) _ ))  = y

-- | Verifies if a list of players is composed just by Pacmans
verifyPlayerList :: [Player] -> Bool
verifyPlayerList [] = True
verifyPlayerList (Pacman (PacState (i,(px,py),v,o,p,l) t m pm):xs) = verifyPlayerList xs
verifyPlayerList _ = False

-- | Makes a play with the intention to get close to Pacmans
chaseMode :: State -> Int -> Play
chaseMode (State m (Ghost (GhoState (i,(x,y),_,_,_,_) _):xs) le) id | id == i   = bestPlayChase (x,y) id xs m
                                                                    | otherwise = chaseMode (State m xs le) id
chaseMode (State m (p:ps) le) id = chaseMode (State m (ps ++ [p]) le) id

-- | Analyzes what Pacman is closer the Ghost and decides what play to do based on the coordinates of the Ghost and the closest Pacman when the ghostMode is Alive
bestPlayChase :: (Int,Int) -> Int -> [Player] -> Maze -> Play
bestPlayChase (gx,gy) id lp m | cy < gy && pieceCoords (gy,gx+1)   m /= Wall && pm == Normal && haveWalls ptC = Move id L
                              | cy > gy && pieceCoords (gy+2,gx+1) m /= Wall && pm == Normal && haveWalls ptC = Move id R
                              | cx < gx && pieceCoords (gy+1,gx)   m /= Wall && pm == Normal = Move id U
                              | cx > gx && pieceCoords (gy+1,gx+2) m /= Wall && pm == Normal = Move id D
                              | cy < gy || cy > gy && pm == Normal = verticalPlayChase pu pd pl pr dv id
                              | cx < gx || cx > gx && pm == Normal = horizontalPlay pu pd pl pr id dv Alive
                              | pm == Mega = bestPlayScatter (gx,gy) id lp m
                              | otherwise = Move id R
              where
                   (cx,cy) = closestPacman lp (gx,gy) (px,py)
                   (px,py) = firstPacman lp 
                   pm = getPacMode lp (px,py)
                   pl = pieceCoords (gy,gx+1)   m -- Left Piece
                   pr = pieceCoords (gy+2,gx+1) m -- Right Piece
                   pu = pieceCoords (gy+1,gx)   m -- Up Piece
                   pd = pieceCoords (gy+1,gx+2) m -- Down Piece
                   dv = gx - cx                   -- Diference between the height of ghost and the closest pacman
                   dc = gy - py                   -- The length between the pacman and the closest ghost
                   ptC = getColumn m gx (cx,cy)   -- Gets a little part of one specific corridor

-- | Makes the best vertical play for the Ghost when ghosMode is Alive
verticalPlayChase :: Piece -> Piece -> Piece -> Piece -> Int -> Int -> Play
verticalPlayChase pu pd pl pr dv id | dv < 0 && pd /= Wall = Move id D
                                    | dv > 0 && pu /= Wall = Move id U
                                    | otherwise = horizontalPlay pu pd pl pr id dv Alive 

-- | Makes the best horizontal play for a Ghost (or a Pacman when used in Tarefa6) when given the ID of the player
horizontalPlay :: Piece -> Piece -> Piece -> Piece -> Int -> Int  -> GhostMode-> Play
horizontalPlay pu pd pl pr id dv gm | pl /= Wall = Move id L
                                    | pr /= Wall = Move id R
                                    | dv < 0 && pd /= Wall && gm == Alive = Move id D
                                    | dv > 0 && pu /= Wall && gm == Alive = Move id U 
                                    | dv < 0 && pu /= Wall && gm == Dead = Move id U
                                    | otherwise = Move id D

-- | Analyzes what Pacman is closer the Ghost and decides what play to do based on the coordinates of the Ghost and the closest Pacman when the ghostMode is Dead
scatterMode :: State -> Int -> Play
scatterMode (State m (Ghost (GhoState (i,(gx,gy),_,_,_,_) _):xs) le) id | id == i   = bestPlayScatter (gx,gy) id xs m
                                                                        | otherwise = scatterMode (State m xs le) id
scatterMode (State m (p:ps) le) id = scatterMode (State m (ps ++ [p]) le) id

-- | Makes the best play for a Ghost is in Dead ghostMode 
bestPlayScatter :: (Int,Int) -> Int -> [Player] -> Maze -> Play
bestPlayScatter (gx,gy) id lp m | closeCorner m (gx,gy)  = playCorner (gx,gy) (px,py) id
                                | cy < gy && pieceCoords (gy+2,gx+1) m /= Wall = Move id R
                                | cy > gy && pieceCoords (gy,gx+1)   m /= Wall = Move id L
                                | cx < gx && pieceCoords (gy+1,gx+2) m /= Wall = Move id D
                                | cx > gx && pieceCoords (gy+1,gx)   m /= Wall = Move id U
                                | cy < gy || cy > gy = verticalPlayScatter pu pd pl pr dv id
                                | cx < gx || cx > gx = horizontalPlay pu pd pl pr id dv Dead
              where
                   (cx,cy) = closestPacman lp (gx,gy) (px,py)
                   (px,py) = firstPacman lp
                   pl = pieceCoords (gy,gx+1)   m -- Left Piece
                   pr = pieceCoords (gy+2,gx+1) m -- Right Piece
                   pu = pieceCoords (gy+1,gx)   m -- Up Piece
                   pd = pieceCoords (gy+1,gx+2) m -- Down Piece
                   dv = gx - cx                 -- Diference between the height of ghost and the closest pacman 
                   cC = cornersCoords m 

-- | Given a maze and the coordinates of player, checks if that player is near a corner of the given maze
closeCorner :: Maze -> Coords -> Bool
closeCorner m (x,y) | elemsCoords [0,1,2] (filter (<=2) (map (distanceCoords (x,y)) cc)) = True
                    | otherwise = False
                where
                  cc = cornersCoords m


-- | Gets the coordinates of the corners of the maze
cornersCoords :: Maze -> [Coords]
cornersCoords m = [(0,0),(0,lc),(lh,0),(lh,lc)]
        where
          lc = length (head m) - 2
          lh = length m - 2                  

-- | Makes the best vertical play for the Ghost in Dead ghostMode
verticalPlayScatter :: Piece -> Piece -> Piece -> Piece -> Int -> Int  -> Play
verticalPlayScatter pu pd pl pr dv id | dv < 0 && pu /= Wall = Move id U
                                      | dv > 0 && pd /= Wall = Move id D
                                      | otherwise = horizontalPlay pu pd pl pr id dv Dead 



-- | Gives the coordinates of the first Pacman of a list of players
firstPacman :: [Player] -> Coords
firstPacman [] = (0,0)
firstPacman (Pacman (PacState (_,(px,py),_,_,_,_) _ _ _):xs) = (px,py)
firstPacman ( _ : xs) = firstPacman xs

-- | Verifies which is the closest Pacman to a Ghost
closestPacman :: [Player] -> Coords -> Coords -> Coords
closestPacman [] _ (cx,cy) = (cx,cy)
closestPacman (Pacman (PacState (_,(px,py),_,_,_,_) _ _ _):xs) (gx,gy) (cx,cy) | od >= nd  = closestPacman xs (gx,gy) (px,py)
                                                                               | otherwise = closestPacman xs (gx,gy) (cx,cy)
                      where
                           nd = distanceCoords (gx,gy) (px,py) -- new distance
                           od = distanceCoords (gx,gy) (cx,cy) -- old distance
closestPacman ( _ : xs) (gx,gy) (cx,cy) = closestPacman xs (gx,gy) (cx,cy)


-- | Calculates distance between a Pacman and the closest Ghost using Taxicab Geometry
distanceCoords :: Coords -> Coords -> Int
distanceCoords (px,py) (gx,gy) = abs (px - gx) + abs (py - gx)


-- | Gets the coordinates of the Ghost house
houseCoords :: Maze -> [Coords]
houseCoords [] = []
houseCoords m@(c:cs) | odd l = [(x-2,y),(x-2,y+1),(x-2,y+2),(x-1,y),(x-1,y+1),(x-1,y+2),(x,y-2),(x,y-1),(x,y),(x,y+1),(x,y+2),(x,y+3),(x,y+4)]
                     | otherwise = [(x-2,y-1),(x-2,y),(x-1,y-1),(x-1,y),(x,y-3),(x,y-2),(x,y-1),(x,y),(x,y+1),(x,y+2)]
      where
        (x,y) = newCoordsGhost m
        l = length c

-- | Makes the best play when the Ghost is inside the Ghost house
playHouse :: Piece -> Int -> Int -> Int -> Int -> Coords -> [Coords] -> Play
playHouse pu dp dv dvp id gc hc | elem gc ohc && dvp > 0 = Move id U
                                | elem gc ohc && dp < 0 = Move id R
                                | elem gc ohc = Move id L
                                | pu == Wall && dv < 0 = Move id R
                                | pu == Wall && dv > 0 = Move id L
                                | otherwise = Move id U
                      where
                        x1 = fst (head hc)
                        ohc = outsidehouse hc x1

-- | Verifies if there's a Wall on a list of Pieces
haveWalls :: [Piece] -> Bool
haveWalls [] = True
haveWalls p = elem Empty p || elem (Food Big) p || elem (Food Little) p

-- | Get the list of pieces on the column of where the Ghost is
getColumn :: Maze -> Int -> Coords -> [Piece]
getColumn m px (ox,oy) = getPart (mt !! oy) px ox           
            where
              mt = transposeMaze m

-- | Gets a part of the column of pieces where the opponent (Ghost if the player is a Pacman, Pacman if the player is a Ghost) is, with that part being located between the the height of the Player and the height the opponent 
getPart :: Corridor -> Int -> Int -> Corridor
getPart [] _ _ = []
getPart _ 0 0  = []
getPart (x:xs) 0 ox = x : getPart xs 0 (ox-1)
getPart (x:xs) px 0 = x : getPart xs (px-1) 0
getPart (x:xs) px ox = getPart xs (px-1) (ox-1) 

-- | Transpoze a maze (swaps columns with the corridors)
transposeMaze :: Maze -> Maze
transposeMaze [] = []
transposeMaze m | null (head m) = []
                | otherwise     = (map head m) : transposeMaze (map tail m) 

-- | Returns the coordinates of the entrance of the Ghost house when given a list of coordinates and an Int
outsidehouse :: [Coords] -> Int -> [Coords]
outsidehouse [] _ = []
outsidehouse (x:xs) n | fst x == n = x : outsidehouse xs n
                      | otherwise = []

-- | Verifies if a player is close to the tunnel 
closeTunnel :: Maze -> Coords -> Bool
closeTunnel m (x,y) | elemsCoords [0,1,2] (filter (<=2) (map (distanceCoords (x,y)) cc)) = True
                    | otherwise = False
                where
                  cc = getCoordsTunnel m

-- | Gets the coordinates of the tunnel
getCoordsTunnel :: Maze -> [Coords]
getCoordsTunnel m | odd l = [(div l 2,0),(div l 2,lc - 1)]
                  | even l = [(div l 2 - 1,0),(div l 2, 0),(div l 2 - 1, lc -1),(div l 2,lc - 1)]
            where
              l = length m
              lc = length (head m)

-- | Verifies if there is at least one element of the first list in the second list
elemsCoords :: [Int] -> [Int] -> Bool
elemsCoords [] _ = True
elemsCoords _ [] = False
elemsCoords (x:xs) cy | elem x cy = True
                      | otherwise = elemsCoords xs cy

-- | Makes plays when the Ghosts are close to the tunnel and they want to get close to the opponent
playTunnelChase :: Maze -> Int -> [Player] -> Coords -> Coords -> Piece -> Piece -> Play
playTunnelChase m id lp (gx,gy) (cx,cy) pr pl | dp > 0 && (gx,gy) == tunnelL && pm == Normal = Move id R 
                                              | dp < 0 && (gx,gy) == tunnelS && pm == Normal = Move id L
                                              | dt == 0 && gy > mm && pr /= Wall && pm == Normal = Move id R
                                              | dt == 0 && gy < mm && pl /= Wall && pm == Normal = Move id L
                                              | dt < 0 && pm == Normal = Move id D
                                              | dt > 0 && pm == Normal = Move id U
                                              | otherwise = bestPlayScatter (gx,gy) id lp m
                where
                  pm = getPacMode lp (cx,cy)
                  dp = gy - cy
                  mm = div (length (head m)) 2
                  tunnelL = last (getCoordsTunnel m)
                  tunnelS = head (getCoordsTunnel m)
                  dt = gx - fst tunnelL


-- | Verifies if there is a Pacman close to the tunnel
getCloseTunnelP :: Maze -> [Player] -> Coords
getCloseTunnelP _ [] = (0,0)
getCloseTunnelP m (Pacman (PacState (_,(px,py),_,_,_,_) _ _ _):xs) | closeTunnel m (px,py) = (px,py)
                                                                   | otherwise = getCloseTunnelP m xs
getCloseTunnelP m (_:ps) = getCloseTunnelP m ps

-- | Makes plays when the Players are close to the tunnel and they want to get away to the opponent
playTunnelScatter :: Maze -> Int -> Coords -> Coords -> Piece -> Piece -> Play
playTunnelScatter m id (gx,gy) (cx,cy) pr pl | gy == 0 = Move id R
                                             | gy == lc - 1 = Move id L
                                             | dp > 0 && pr /= Wall && dt /= 0 = Move id R
                                             | dp < 0 && pl /= Wall && dt /= 0 = Move id L
                                             | dv < 0 = Move id U
                                             | dv > 0 = Move id D
                where
                  dp = gy - cy
                  dv = gx - cx
                  mm = div (length (head m)) 2
                  tunnelL = last (getCoordsTunnel m)
                  tunnelS = head (getCoordsTunnel m)
                  dt = gx - fst tunnelL
                  lc = length (head m)

-- | Makes plays when a player is in the corner when given the coordinates of the opponent
playCorner :: Coords -> Coords -> Int  -> Play
playCorner (1,1) (x,y) id   | x < y     = Move id D
                            | otherwise = Move id R
playCorner (1,lc) (x,y) id  | x < y     = Move id D
                            | otherwise = Move id L
playCorner (lh,1) (x,y) id  | x < y     = Move id U
                            | otherwise = Move id R
playCorner (lh,lc) (x,y) id | x < y     = Move id U
                            | otherwise = Move id L

-- | If more than one Ghost are on the same coordinates this function gives a different play to the first of those Ghosts on a list of players, so they don't overlap
differentPlay :: (Int,Int) -> Int -> [Player] -> Maze  -> GhostMode -> Play
differentPlay (gx,gy) id lp m Alive | cx < gx && pieceCoords (gy+1,gx)   m /= Wall = Move id U
                                    | cx > gx && pieceCoords (gy+1,gx+2) m /= Wall = Move id D
                                    | cy < gy && pieceCoords (gy,gx+1)   m /= Wall = Move id L
                                    | cy > gy && pieceCoords (gy+2,gx+1) m /= Wall = Move id R
                                    | cx < gx || cx > gx = horizontalPlay pu pd pl pr id dv Alive
                                    | cy < gy || cy > gy = verticalPlayChase pu pd pl pr dv id
              where
                   (cx,cy) = closestPacman lp (gx,gy) (px,py)
                   (px,py) = firstPacman lp 
                   pl = pieceCoords (gy,gx+1)   m -- Left Piece
                   pr = pieceCoords (gy+2,gx+1) m -- Right Piece
                   pu = pieceCoords (gy+1,gx)   m -- Up Piece
                   pd = pieceCoords (gy+1,gx+2) m -- Down Piece
                   dv = gx - cx                   -- Diference between the height of ghost and the closest pacman
differentPlay (gx,gy) id lp m Dead | cx < gx && pieceCoords (gy+1,gx+2) m /= Wall = Move id D
                                   | cx > gx && pieceCoords (gy+1,gx)   m /= Wall = Move id U
                                   | cy < gy && pieceCoords (gy+2,gx+1) m /= Wall = Move id R
                                   | cy > gy && pieceCoords (gy,gx+1)   m /= Wall = Move id L
                                   | cx < gx || cx > gx = horizontalPlay pu pd pl pr id dv Dead
                                   | cy < gy || cy > gy = verticalPlayScatter pu pd pl pr dv id
              where
                   (cx,cy) = closestPacman lp (gx,gy) (px,py)
                   (px,py) = firstPacman lp
                   pl = pieceCoords (gy,gx+1)   m -- Left Piece
                   pr = pieceCoords (gy+2,gx+1) m -- Right Piece
                   pu = pieceCoords (gy+1,gx)   m -- Up Piece
                   pd = pieceCoords (gy+1,gx+2) m -- Down Piece
                   dv = gx - cx                 -- Diference between the height of ghost and the closest pacman 

-- | Given a list of players and the coordinates of a player returns the PacMode of the player
getPacMode :: [Player] -> Coords -> PacMode
getPacMode ((Pacman (PacState (i,(x,y),v,o,p,l) t m pm)):xs) (px,py) | px == x && py == y = pm
                                                                     | otherwise = getPacMode xs (px,py)
getPacMode (_:xs) c = getPacMode xs c