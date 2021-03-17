{- |

= Introduction of the Task:

This was the last task that we were assigned, excluding the usage of gloss to make a graphical interface.
Here we were tasked with creating an artificial inteligence similar to the one on Tarefa5, but for a Pacman.
There were many scenarios we had to consider to try and make the most efficient AI possible.

= Objectives of the task:

1- Get the bot go closer to Big Foods if there are no Ghosts to make it loose a life.

2- Make the best play when the bot is close to the tunnel.

3- Make the bot decide which will be the best play based on his PacMode.

4- Make it so the bot tries to eat Little Food until he is close to the Ghost and it might loose a life.


= How we solved these issues?

1- If the ID given does not exist return Nothing

2- If there is a Ghost close to the tunnel and the Pacman is also close to tunnel:

2.1- If his PacMode is Mega makes a play given by the function playTunnelMega which which needs the maze, 
     the ID of the Pacman, the coordinates of the Pacman, the coordinates of the Ghost closest to the tunnel,
     the pieces to right and the left of the Pacman in to work.

2.2- If his PacMode is Normal makes the play given by the function playTunnelScatter (definied in Tarefa5) which 
     needs the maze, the ID of the Pacman, the coordinates of the Pacman, the coordinates of the ghost close to 
     the tunnel, he pieces to right and the left of the Pacman in order to work.

3- If the Pacman is closer to the Big Food than the closest Ghost to the Big Food makes the play returned by the 
   function playPacmanMega, that is given the coordinates of the Pacman, the coordinates of the Big Food, the ID of 
   the bot and the maze.

4- If the PacMode from the Pacman is Mega and the previous point were not verified he makes the play returned by the 
   function  playPacmanMega, that is given the coordinates of the Pacman, the coordinates of the closest Ghost to the
   Pacman, the ID of the bot and the maze.
   On this function, playPacmanMega, the play prioritizes horizontal plays rather than vertical ones, but
   if a part of the column where the closest Ghost is, with that part being located between the the height
   of the Player and the height the Ghost, is only composed by walls then the function will make a vertical play. 

5- In every case If the Orientation of the play is the same as the Pacman's return Nothing, otherwise Just the returns 
   the best play based on the previuos .
   conditions

= Discussion and Conclusion

Like Tarefa5, this was also one difficult task and sometimes we did not even know were to look for solutions, but with
time, we were able to locate our problems and found solutions.
We are also proud of the results of this task, as our bot works as intended in most cases.
We liked this task since we worked more with artificial inteligence and it was a good challenge and experience.

-}


module Tarefa6 where
import Types
import Tarefa2
import Tarefa4
import Tarefa5

teste1T6 :: Maybe Play
teste1T6 = bot 0 (State mazeTeste2 jsTeste1T5 2)


teste2T6 :: Maybe Play
teste2T6 = bot 0 (State mazeTeste2 jsTeste2T5 2)


teste3T6 :: Maybe Play
teste3T6 = bot 0 (State mazeTeste2 jsTeste3T5 2)



-- | Main Function of Tarefa6 given an ID and a State makes the best play possible for a Pacman according to it's PacMode and specific situations
bot :: Int -> State -> Maybe Play
bot _  (State m [] le) = Nothing
bot id (State m pl le) | idPacmanNotExist pl id = Nothing
                       | (tx,ty) /= (0,0) && closeTunnel m (px,py) && getPStateId pl id == Mega && compareOrientation o ptM = Just ptM
                       | (tx,ty) /= (0,0) && closeTunnel m (px,py) && getPStateId pl id == Mega = Nothing
                       | (tx,ty) /= (0,0) && closeTunnel m (px,py) && getPStateId pl id == Normal && compareOrientation o ptS = Just ptS
                       | (tx,ty) /= (0,0) && closeTunnel m (px,py) && getPStateId pl id == Normal = Nothing                       
                       | closestPacGho (px,py) (gxf,gyf) (fbx,fby) && compareOrientation o pf && (ffbx,ffby) /= (0,0) = Just pf
                       | closestPacGho (px,py) (gxf,gyf) (fbx,fby) && (ffbx,ffby) /= (0,0) = Nothing 
                       | getPStateId pl id == Mega   && compareOrientation o pm = Just pm
                       | getPStateId pl id == Normal && compareOrientation o pfl = Just pfl 
                       | otherwise = Nothing 
    where
       o = getIdOrientation pl id                        -- Gets the orientation of the Pacman
       (px,py) = getIdCoords pl id                       -- Gets the coordinates of the Pacman
       (fx,fy) = firstGhost pl                           -- Gets the coordinates of the first Ghost on the list of players
       lf = foodBigCoords m 0                            -- Gets the list of the all Big Foods in maze
       (ffbx,ffby) = firstFoodBig m (0,0)                -- Gets the Coordinates of the First Big Food
       (fbx,fby) = closestFoodBig (px,py) lf (ffbx,ffby) -- Closest Big Food to the Pacman
       (gxf,gyf) = closestGhost pl (fbx,fby) (fx,fy) m   -- Closest Ghost to the Closest Big Food
       (gx,gy) = closestGhost pl (px,py) (fx,fy) m       -- Closest Ghost to the Pacman
       pm = playPacmanMega (px,py) (gx,gy) m id          -- Play when the PacMode is Mega
       pf = playPacmanMega (px,py) (fbx,fby) m id        -- Play when the Pacman wants to get closer to the closest Big Food
       pL = pieceCoords (gy,gx+1)   m                    -- Left Piece
       pr = pieceCoords (gy+2,gx+1) m                    -- Right Piece
       pfl = playFoodLittle m (px,py) (gx,gy) id         -- Normal play when the Pacman is on pacMode Normal
       (tx,ty) = getCloseTunnelG m pl                    -- Coords of the closest Ghost to the tunnel
       ptM = playTunnelMega m id (px,py) (tx,ty) pr pL  -- Play when the pacman is close to the tunnel and his PacMode is Mega
       ptS = playTunnelScatter m id (px,py) (tx,ty) pr pL-- Play when the pacman is close to the tunnel and his PacMode is Normal

-- | Recieves an ID and checks what type of player the id belongs to, returns False if the ID belongs to a Pacman and True if it belongs to a Ghost
idPacmanNotExist :: [Player] -> Int -> Bool
idPacmanNotExist [] _ = True
idPacmanNotExist (Pacman (PacState (i,(_,_),_,_,_,_) _ _ _):xs) id | id == i = False
                                                                   | otherwise  = idPacmanNotExist xs id
idPacmanNotExist (Ghost (GhoState (i,(_,_),_,_,_,_) _ ):xs) id | id == i = True
                                                               | otherwise = idPacmanNotExist xs id

-- | Given a list of Players and an ID of a Pacman returns the PacMode of the Pacman
getPStateId :: [Player] -> Int -> PacMode
getPStateId (Pacman (PacState (i,(_,_),_,_,_,_) _ _ pm):xs) id | id == i = pm
                                                               | otherwise = getPStateId xs id 
getPStateId (_:ps) id = getPStateId ps id

-- | Gives the coordinates of the first Ghost that is on the list of players 
firstGhost :: [Player] -> Coords
firstGhost (Ghost (GhoState (_,(gx,gy),_,_,_,_) _):xs) = (gx,gy)
firstGhost ( _ : xs) = firstPacman xs

-- | Chooses the best play for the Pacman when its PacMode is Normal
playPacmanNormal :: Coords -> Coords -> Maze -> Int -> Play
playPacmanNormal (px,py) (gx,gy) m id | elem (px,py) cC = playCorner (px,py) (gx,gy) id
                                      | gy < py && pieceCoords (py+2,px+1) m /= Wall = Move id R
                                      | gy > py && pieceCoords (py,px+1)   m /= Wall = Move id L
                                      | gx < px && pieceCoords (py+1,px+2) m /= Wall = Move id D
                                      | gx > px && pieceCoords (py+1,px)   m /= Wall = Move id U
                                      | gy < py || gy > py  = verticalPlayNormal pu pd dv dc id
                                      | gx < px || gx > px  = horizontalPacman pu pd pl pr id dv Normal
                                      | pr /= Wall = Move id R
                                      | pl /= Wall = Move id L
                                      | pu /= Wall = Move id U
                                      | pd /= Wall = Move id D
                where 
                   pl = pieceCoords (py,px+1)   m -- Left Piece
                   pr = pieceCoords (py+2,px+1) m -- Right Piece
                   pu = pieceCoords (py+1,px)   m -- Up Piece
                   pd = pieceCoords (py+1,px+2) m -- Down Piece
                   dv = px - gx                   -- Diference between the height of pacman and the closest ghost
                   dc = py - gy                   -- The length between the pacman and the closest ghost
                   cC = cornersCoords m 

-- | Makes the best vertical play for the Pacman when its PacMode is Normal
verticalPlayNormal :: Piece -> Piece -> Int -> Int -> Int -> Play
verticalPlayNormal pu pd dv dc id | dv < 0 && pu /= Wall = Move id U
                                  | dv > 0 && pd /= Wall = Move id D
                                  | dc < 0               = Move id R
                                  | dc > 0               = Move id L

-- | Makes the best horizontal play for both PacModes
horizontalPacman :: Piece -> Piece -> Piece -> Piece -> Int -> Int  -> PacMode-> Play
horizontalPacman pu pd pl pr id dv pm | pl /= Wall = Move id L
                                      | pr /= Wall = Move id R
                                      | dv < 0 && pd /= Wall && pm == Mega = Move id D
                                      | dv > 0 && pu /= Wall && pm == Mega = Move id U 
                                      | dv < 0 && pu /= Wall && pm == Normal = Move id U
                                      | otherwise = Move id D


-- | Chooses the best play for a Pacman when its PacMode is Mega or when Pacman wants to get closer to the closest Food Big
playPacmanMega :: Coords -> Coords -> Maze -> Int -> Play
playPacmanMega (px,py) (gx,gy) m id | gy < py && pieceCoords (py,px+1)   m /= Wall && haveWalls ptC = Move id L
                                    | gy > py && pieceCoords (py+2,px+1) m /= Wall && haveWalls ptC = Move id R
                                    | gx < px && pieceCoords (py+1,px)   m /= Wall = Move id U
                                    | gx > px && pieceCoords (py+1,px+2) m /= Wall = Move id D
                                    | gy < py || gy > py = verticalPlayMega pu pd dv dc id
                                    | gx < px || gx > px = horizontalPacman pu pd pl pr id dv Mega
                                    | pr /= Wall = Move id R
                                    | pl /= Wall = Move id L
                                    | pu /= Wall = Move id U
                                    | pd /= Wall = Move id D
                where 
                   pl = pieceCoords (py,px+1)   m -- Left Piece
                   pr = pieceCoords (py+2,px+1) m -- Right Piece
                   pu = pieceCoords (py+1,px)   m -- Up Piece
                   pd = pieceCoords (py+1,px+2) m -- Down Piece
                   dv = px - gx                   -- Diference between the height of pacman and the closest ghost
                   dc = py - gy                   -- The length between the pacman and the closest ghost
                   ptC = getColumn m px (gx,gy)         -- Gets a little part of one specific corridor


-- | Makes the best vertical play for a Pacman when its Pacmode is Mega
verticalPlayMega :: Piece -> Piece -> Int -> Int -> Int -> Play
verticalPlayMega pu pd dv dc id | dv < 0 && pd /= Wall = Move id D
                                | dv > 0 && pu /= Wall = Move id U
                                | dc < 0               = Move id L
                                | dc > 0               = Move id R

-- | Compares if two orientations are different
compareOrientation :: Orientation -> Play -> Bool
compareOrientation o (Move id or) = o /= or


-- | Given a list of players and an ID gets the coordinates of the pacman with that ID
getIdCoords :: [Player] -> Int -> Coords
getIdCoords (Pacman (PacState (i,(px,py),_,_,_,_) _ _ _):xs) id | id == i = (px,py)
                                                                | otherwise = getIdCoords xs id
getIdCoords (_:ps) id = getIdCoords ps id  

-- | Checks which is the closest Ghost to the Pacman 
closestGhost :: [Player] -> Coords -> Coords -> Maze -> Coords
closestGhost [] _ (cx,cy) _ = (cx,cy)
closestGhost (Ghost (GhoState (_,(gx,gy),_,_,_,_) _):xs) (px,py) (cx,cy) m | od >= nd  && gH = closestGhost xs (px,py) (gx,gy) m
                                                                           | otherwise = closestGhost xs (px,py) (cx,cy) m
                      where
                           nd = distanceCoords (px,py) (gx,gy) -- new distance
                           od = distanceCoords (px,py) (cx,cy) -- old distance
                           lc = houseCoords m
                           gH = moveGhostHouse (gx,gy) lc
closestGhost ( _ : xs) (px,py) (cx,cy) m = closestGhost xs (px,py) (cx,cy) m

-- | Gives the coordinates of every Big Food on the maze
foodBigCoords :: Maze -> Int -> [Coords]
foodBigCoords [] _ = []
foodBigCoords (p:ps) x | elem (Food Big) p = (foodBigCorridor 0 p x) ++ cpm
                       | otherwise         = foodBigCoords ps (x+1)
              where
                cpm =  foodBigCoords ps (x+1)

-- | Gives the coordinates of every Big Food on a corridor
foodBigCorridor :: Int -> Corridor -> Int -> [Coords]
foodBigCorridor _ [] _ = []
foodBigCorridor y (p:ps) x | p == Food Big = (c:cpc)
                           | otherwise     = cpc
                    where
                      c = (x,y)
                      cpc = foodBigCorridor (y+1) ps x

-- | Gives the coordinates of the closest Big Food to a Pacman
closestFoodBig :: Coords -> [Coords] -> Coords -> Coords
closestFoodBig _ [] (x,y) = (x,y)
closestFoodBig (px,py) ((x,y):xs) (fbx,fby) | nd < od   = closestFoodBig (px,py) xs (x,y)
                                            | otherwise = closestFoodBig (px,py) xs (fbx,fby)
                              where
                                nd = distanceCoords (px,py) (x,y)
                                od = distanceCoords (px,py) (fbx,fby)

-- | Checks if a Pacman is closer to the Big Food than a Ghost
closestPacGho :: Coords -> Coords -> Coords -> Bool
closestPacGho (px,py) (gx,gy) (fbx,fby) = dp < dg  
                        where
                          dp = distanceCoords (fbx,fby) (px,py)
                          dg = distanceCoords (fbx,fby) (gx,gy)

-- | Gets the first Big Food coordinates
firstFoodBig :: Maze -> Coords -> Coords
firstFoodBig [] _ = (0,0)
firstFoodBig ([]:xs) (x,y) = firstFoodBig xs (x+1,0)
firstFoodBig ((f:fs):xs) (x,y) | f == Food Big = (x,y)
                               | otherwise = firstFoodBig (fs:xs) (x,y+1)

-- | Analyzes if there a Ghost in the house
moveGhostHouse :: Coords -> [Coords] -> Bool
moveGhostHouse _ [] = True
moveGhostHouse (gx,gy) c | elem (gx,gy) c = False
                         | otherwise      = True

-- | Verifies if there is a Ghost close to the Tunnel
getCloseTunnelG :: Maze -> [Player] -> Coords
getCloseTunnelG _ [] = (0,0)
getCloseTunnelG m (Ghost (GhoState (_,(gx,gy),_,_,_,_) _ ):xs) | closeTunnel m (gx,gy) = (gx,gy)
                                                               | otherwise = getCloseTunnelG m xs
getCloseTunnelG m (_:ps) = getCloseTunnelG m ps


-- | Makes the best possible play for a Pacman when it's worth going after a Little Food, when it's not safe, does playPacmanNormal so that Pacman gets away from a Ghost
playFoodLittle :: Maze -> Coords -> Coords -> Int -> Play
playFoodLittle m (px,py) (gx,gy) id | cl /= (0,0) && distanceCoords (px,py) (gx,gy) >= 3  = Move id L
                                    | cr /= (0,0) && distanceCoords (px,py) (gx,gy) >= 3 = Move id R
                                    | cu /= (0,0) && distanceCoords (px,py) (gx,gy) >= 3 = Move id U
                                    | cd /= (0,0) && distanceCoords (px,py) (gx,gy) >= 3 = Move id D
                                    | otherwise = playPacmanNormal (px,py) (gx,gy) m id
       where
        pl = pieceCoords (py,px+1)   m -- Left Piece
        pr = pieceCoords (py+2,px+1) m -- Right Piece
        pu = pieceCoords (py+1,px)   m -- Up Piece
        pd = pieceCoords (py+1,px+2) m -- Down Piece
        cl = coordsFL (px,py) pl L
        cr = coordsFL (px,py) pr R
        cu = coordsFL (px,py) pu U
        cd = coordsFL (px,py) pd D



-- | Given the coordinates of a Pacman and Little Food gives the coordinates where Pacman will end up after eating the Food
coordsFL :: Coords -> Piece -> Orientation -> Coords
coordsFL (px,py) (Food Little) R = (px,py+1)
coordsFL (px,py) (Food Little) L = (px,py-1)
coordsFL (px,py) (Food Little) U = (px-1,py)
coordsFL (px,py) (Food Little) D = (px+1,py)
coordsFL _ _ _ = (0,0)


-- | Plays when the Pacman wants to enter in the tunnel on Mega PacMode 
playTunnelMega :: Maze -> Int -> Coords -> Coords -> Piece -> Piece -> Play
playTunnelMega m id (gx,gy) (cx,cy) pr pl  | dp > 0 && (gx,gy) == tunnelL = Move id R 
                                           | dp < 0 && (gx,gy) == tunnelS = Move id L
                                           | dt == 0 && gy > mm && pr /= Wall = Move id R
                                           | dt == 0 && gy < mm && pl /= Wall = Move id L
                                           | dt < 0 = Move id D
                                           | dt > 0 = Move id U
                where
                  dp = gy - cy
                  mm = div (length (head m)) 2
                  tunnelL = last (getCoordsTunnel m)
                  tunnelS = head (getCoordsTunnel m)
                  dt = gx - fst tunnelL