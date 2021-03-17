import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.Maybe
import Types
import Tarefa4
import Tarefa2

wall :: Picture
wall = color white (Polygon [(0,0),(20,0),(20,20),(0,20)])

foodLittle :: Picture
foodLittle = translate 10 10 (color blue (ThickCircle 0 10))

foodBig :: Picture
foodBig = translate 10 10 (color red (ThickCircle 0 15))

empty :: Picture
empty = translate 10 10 (color black (ThickCircle 0 5))

pacman :: Picture
pacman = translate 10 10 (color yellow (ThickCircle 0 20))

ghost :: Picture
ghost = translate 10 10 (color orange(ThickCircle 0 20))


pieceToPic :: Piece -> Picture
pieceToPic Wall = wall
pieceToPic (Food Little) = foodLittle
pieceToPic (Food Big)    = foodBig
pieceToPic (Empty)       = empty
pieceToPic (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Normal))) = pacman
pieceToPic (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Mega)))   = pacman
pieceToPic (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Dying)))  = pacman
pieceToPic (PacPlayer (Ghost (GhoState (i,c,x,y,z,l) Alive )))      = ghost
pieceToPic (PacPlayer (Ghost (GhoState (i,c,x,y,z,l) Dead )))       = ghost


drawLine :: [Piece] -> Int -> [Picture]
drawLine [] _ = []
drawLine (h:t) x = (translate (fromIntegral(x*20)) 0 (pieceToPic h)):(drawLine t (x+1))

drawMap :: Maze -> (Int,Int) -> [Picture]
drawMap [] _ = []
drawMap (h:t) (x,y) = (translate 0 (fromIntegral(y*(-20))) (pictures (drawLine h x))):(drawMap t (x,y+1))

drawState :: State -> (Int,Int) -> [Picture]
drawState (State m []  _) (x,y) = drawMap m (x,y)
drawState s (x,y) = drawMap (drawCompleteMaze s) (x,y)
 

drawCompleteMaze :: State -> Maze
drawCompleteMaze (State m lp _) = completeMaze m lp


completeMaze :: Maze -> [Player] -> Maze
completeMaze m [] = m
completeMaze m ((Pacman (PacState (i,c,x,y,z,l) o mo pm)):ps) = completeMaze (insertOnMazeP m (PacState (i,c,x,y,z,l) o mo pm)) ps
completeMaze m ((Ghost (GhoState (i,c,x,y,z,l) gm)):ps) = completeMaze (insertOnMazeG m (GhoState (i,c,x,y,z,l) gm)) ps



insertOnMazeP :: Maze -> PacState -> Maze
insertOnMazeP (h:t) (PacState (i,(0,y),p,o,z,l) ti m pm) = (insertCorridorP h (PacState (i,(0,y),p,o,z,l) ti m pm)) : t
insertOnMazeP (h:t) (PacState (i,(x,y),p,o,z,l) ti m pm) = h:insertOnMazeP t (PacState (i,(x-1,y),p,o,z,l) ti m pm)

insertCorridorP :: Corridor -> PacState -> Corridor
insertCorridorP (h:t) (PacState (i,(x,0),p,o,z,l) ti m pm) = PacPlayer (Pacman(PacState (i,(x,0),p,o,z,l) ti m pm)) : t
insertCorridorP (h:t) (PacState (i,(x,y),p,o,z,l) ti m pm) = h:insertCorridorP t (PacState (i,(x,y-1),p,o,z,l) ti m pm)


insertOnMazeG :: Maze -> GhoState -> Maze
insertOnMazeG (h:t) (GhoState (i,(0,y),p,o,z,l) gm) = (insertCorridorG h (GhoState (i,(0,y),p,o,z,l) gm)) : t
insertOnMazeG (h:t) (GhoState (i,(x,y),p,o,z,l) gm) = h:insertOnMazeG t (GhoState (i,(x-1,y),p,o,z,l) gm)

insertCorridorG :: Corridor -> GhoState -> Corridor
insertCorridorG (h:t) (GhoState (i,(x,0),p,o,z,l) gm) = PacPlayer(Ghost(GhoState (i,(x,0),p,o,z,l) gm)) : t
insertCorridorG (h:t) (GhoState (i,(x,y),p,o,z,l) gm) = h:insertCorridorG t (GhoState (i,(x,y-1),p,o,z,l) gm)


reactEvent :: Event -> Manager -> Manager
reactEvent (EventKey (SpecialKey KeyUp) Down _ _)  id (Manager s pid step bf delt del )   = Manager (Tarefa2.play (Move pid U) s) pid step bf delt del
reactEvent (EventKey (SpecialKey KeyDown) Down _ _) id (Manager s pid step bf delt del )  = Manager (Tarefa2.play (Move pid D) s) pid step bf delt del
reactEvent (EventKey (SpecialKey KeyLeft) Down _ _) id (Manager s pid step bf delt del )  = Manager (Tarefa2.play (Move pid L) s) pid step bf delt del
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) id (Manager s pid step bf delt del ) = Manager (Tarefa2.play (Move pid R) s) pid step bf delt del
reactEvent   s = s

mapDrawPic :: Picture
mapDrawPic = pictures (drawState (State mazeGloss jsGloss 1) (0,0))


initialState :: State 
initialState = State mazeGloss jsGloss 1


displayMode :: Display
displayMode = InWindow "Game" (640,640) (0,0)

linePic :: Picture
linePic = Line [((-300),(-300)),((-300),300),(300,300),(300,(-300)),((-300),(-300))]

timeChange :: Float -> Manager -> Manager
timeChange f (Manager s pid step bf delt del ) = Manager (passTime (round f) s) pid step bf delt del

drawInitialState :: State -> Picture
drawInitialState s = pictures (drawState s (0,0))

loadManagerGloss :: Manager
loadManagerGloss = (Manager (State mazeGloss jsGloss 1) 0 0 0 0 1)

drawManager :: Manager -> Picture
drawManager (Manager s pid step bf delt del ) = drawInitialState s

main :: IO()
main  = do Graphics.Gloss.Interface.Pure.Game.play displayMode
           black
           1 
           loadManagerGloss
           drawManager 
           reactEvent
           timeChange
