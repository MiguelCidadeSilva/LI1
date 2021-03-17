{- |

Copyright   : (c) José Carvalho, 2021
                  Miguel Cidade Silva, 2021

= Information

This in the summary of our work on the profect we made for "Laboratórios de Informática 1" where we
made a clone of the popular game Pacman using the Haskell programming language. The students were
divided into groups of two. Our group consists of José Carvalho (a94913) and Miguel Cidade Silva
(a97031) both students of the first year of "Mestrado Integrado em Engenharia Informática" at
University of Minho in the 2020-2021 academic year.
This project started on October 2020 and finished on January 2021.

= Starting Point

The project was split into two phases, each containing three tasks. On the first phase we developed
the general cases that occur during a game of Pacman, created mazes, made plays and compacted mazes.
While during the second phase we were tasked with working with the passage of time through NCurses,
and we developed artificial inteligence for both types of players, Ghosts and Pacman.

= Discussion and Conclusion of the Project

As the first project we made in our university degree this was quite new to us, and we were learning
as we were making the tasks and developing the project.
We are pround of the results of our effort, as when we started we never imagined we would be able to
archieve such a thing right on the first semester of our first year.
As a whole, we were always content while making our game and solving the problems that are part of the
journey of a software developer.
After finishing this project, we strongly believe we choose the correct degree when we analyze our
interests and personalities, as we are eager to do more projects like this one. 

= Introduction of the module:

This is the main module of the project, where everything we worked towards comes together.
Most of it's functions were given to us by the professors and we just had to complete them.
The goal was to simulate a real game of Pacman on the terminal using NCurses.

= Objectives of the task:

1- Complete certain functions.

2- Make the Ghost plays from Tarefa5 occur during gameplay.

= How did we complete this Module?

1- We started to complete the functions updateTime, resetTimer, nextFrame and upgradeControlledPlayer.

1.1- The function updateTime just as the name suggests updates the time on a Manager, before turns
     into now, now is the current time, and delta becomes delta plus the difference between now and
     before.

1.2- The function resetTime turns before on a Manager into now and delta into 0.

1.3- nextFrame calls upon the function passTime defined on Tarefa4, and if necessary, evokes 
     passTimeFracional.

1.4- upgradeControlledPlayer changes the orientation of the player being controlled given the a key
     from the keyboard.

2- We created two functions on this module, one that applies the plays from Tarefa5 and the other applies
   the plays from Tarefa6, it is not necessary to apply the plays from Tarefa6 but we put it in the code
   for testing our bot function from Tarefa6.

3- The loop function is the function that allows us to play the game alongside the main function. We added
   new cases to this function.

3.1- When the lifes of the Player get to 0, the game ends, sometimes on the terminal we noticed the game
     stops with this function not giving any output.

3.2- When there are not any Foods on the maze, the level increases by one loading a new map, we made four
     maps in total, with the first one not being realy a valid maze and just a starting level on which
     Pacman just needs to eat one Big Food.

-}


module Main where
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5 
import Tarefa6

-- | Manager given by the professors
loadManager :: Manager
loadManager = (Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime)

-- * Function made by us
-- | Changes the orientation of a player based on the key inserted on the keyboard
updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyLeftArrow  (Manager state pid step before delta delay) = Manager (applyPlaysGhost (changeOrientation pid L state)) pid step before delta delay
updateControlledPlayer KeyRightArrow (Manager state pid step before delta delay) = Manager (applyPlaysGhost (changeOrientation pid R state)) pid step before delta delay
updateControlledPlayer KeyUpArrow    (Manager state pid step before delta delay) = Manager (applyPlaysGhost (changeOrientation pid U state)) pid step before delta delay
updateControlledPlayer KeyDownArrow  (Manager state pid step before delta delay) = Manager (applyPlaysGhost (changeOrientation pid D state)) pid step before delta delay

-- | Changes orientation of a player on a state given it's ID and the new orientation
changeOrientation :: Int -> Orientation -> State -> State
changeOrientation _ _ (State maze [] le) = State maze [] le
changeOrientation pid orientation (State maze (Ghost (GhoState (i,(cx,cy),v,o,p,l) gm):xs) le) | pid == i = (State maze (Ghost (GhoState (i,(cx,cy),v,orientation,p,l) gm):xs) le)
                                                                                               | idExist pid xs = changeOrientation pid orientation  (State maze (xs ++ [Ghost (GhoState (i,(cx,cy),v,o,p,l) gm)]) le)
                                                                                               | otherwise = (State maze (Ghost (GhoState (i,(cx,cy),v,o,p,l) gm):xs) le) 
changeOrientation pid orientation (State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le) | pid == i       = State maze (Pacman (PacState (i,(cx,cy),v,orientation,p,l) t m pm):xs) le
                                                                                                    | idExist pid xs = changeOrientation pid orientation (State maze (xs ++ [Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm)]) le)
                                                                                                    | otherwise      = State maze (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm):xs) le

-- | Applies plays made on Tarefa5 to the Ghosts on a State
applyPlaysGhost :: State -> State
applyPlaysGhost s = playGhostApply pl s
      where
        pl = getListPlays s

-- | Gets the list of plays for the Ghosts (Tarefa5)
getListPlays :: State -> [Play]
getListPlays s = ghostPlay s

-- | Applies plays made on Tarefa6 to the bot on a State we made this function to test our bot, but we removed it and made it so that humans are the ones playing the game, if needed this can be changed anytime
playGhostApply :: [Play] -> State -> State
playGhostApply [] s = s
playGhostApply (p:ps) s@(State maze pl le) | compareOrientation o p = playGhostApply ps (play p s)
                                           | otherwise = playGhostApply ps s
      where
       id = getIdPlay p
       o = getIdOrientation pl id 

-- | Applies the play to a bot on a State
applyPlayBot :: State -> Int -> State
applyPlayBot s pid | o == Null = s
                   | otherwise = changeOrientation pid o s
  where
    botPlay = bot pid s
    o = getOrientationPlay botPlay

-- | Gets the orientation of a Maybe Play
getOrientationPlay :: Maybe Play -> Orientation
getOrientationPlay Nothing = Null
getOrientationPlay (Just (Move _ o)) = o       

-- | Gets the ID on a Play data type
getIdPlay :: Play -> Int
getIdPlay (Move id _) = id

-- | Updates the time on a Manager
updateTime :: Integer -> Manager -> Manager
updateTime now man@(Manager state pid step before delta delay) = if (before == 0) then Manager state pid step now (delta + delay) delay
                                                                 else Manager state pid step now (delta+(now-before)) delay

-- | Resets the time on a Manager
resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state p step b d delay) = (Manager state p step now 0 delay)



-- | Calculates the number of steps to do in the function passTime
numberSteps :: Integer -> Integer -> Int
numberSteps delta delay = fromInteger (div delta delay)


-- * Functions given by the professors
-- | Updates the screen given the window the color and the manager
updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man = do updateWindow w $ do -- clear
                                              setColor a
                                              moveCursor 0 0 
                                              drawString $ show (state man)
                          render
-- | Gives the current time
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime


-- | Loop function that allows the game to run
loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if (getLife man <= 0 || getLevel man > 4)
    then return ()
    else if (existFood s)
            then if ( delt > del )
                    then loop w $ nextFrame now man
                    else do
                          ev <- getEvent w $ Just 0
                          case ev of
                                Nothing -> loop w (updateTime now man)
                                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                                Just ev' ->
                                   if (ev' == EventCharacter 'q')
                                      then return ()
                                      else loop w (updateTime now man)
            else loop w (upgradeLevel man)

-- | Main function of the project
main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadOurManager

-- | Gets the number of lives of a player when given a Manager 
getLife :: Manager -> Int
getLife (Manager (State m lp le) pid _ _ _ _) = foldr (\x acc -> (getLifePlayer x pid) + acc) 0 lp

-- | Gets the number of lives given one Player
getLifePlayer :: Player -> Int -> Int
getLifePlayer (Pacman (PacState (i,(cx,cy),v,o,p,l) t m pm)) id | id == i = l
                                                                | otherwise = 0
getLifePlayer _ _ = 0

-- | Verifies if there are any Foods on a maze
existFood :: State -> Bool
existFood (State m lp le) = foldr (\x acc -> existFoodCorridor x || acc) (False) m

-- | Verifies if there are Foods on the corridor
existFoodCorridor :: Corridor -> Bool
existFoodCorridor t = foldr (\x acc -> if (x == Food Big || x == Food Little) then True || acc else False || acc) (False) t 

-- | Increase the level of the manager
upgradeLevel :: Manager -> Manager
upgradeLevel (Manager (State maze pl le) pid step before delta delay) | le == 1 = (Manager sTN1 pid 0 0 0 delay)
                                                                      | le == 2 = (Manager sTN2 pid 0 0 0 delay)
                                                                      | le == 3 = (Manager sTN3 pid 0 0 0 delay)
                                                                      | otherwise = (Manager (State maze pl 5) pid step before delta delay)
       where

          pln = map increaseTheVelocity pl
          sTO1 = State mazeTeste2 pln (le+1)
          sTN1 = restartGame sTO1
          sTO2 = State mazeTeste3 pln (le+1)
          sTN2 = restartGame sTO2
          sTO3 = State mazeTeste4 pln (le+1)
          sTN3 = restartGame sTO3

-- | Given a Manager returns the level of the State of the game
getLevel :: Manager -> Int
getLevel (Manager (State maze pl le) pid step before delta delay)  = le

-- | Changes the velocity of a Ghost to 1 when the level of the game increases
increaseTheVelocity :: Player -> Player
increaseTheVelocity (Ghost (GhoState (i,(cx,cy),v,o,p,l) gm)) = (Ghost (GhoState (i,(cx,cy),1,o,p,l) gm))
increaseTheVelocity p = p



---------------Player--------------


-- | Manager we made for testing
loadOurManager :: Manager
loadOurManager = (Manager sT1 0 0 0 0 defaultDelayTime)


-- | Determines what the next frame of the game will be and returns it on a Manager
nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step before delta delay) | odd step && odd nsteps = Manager (applyPlaysGhost (passTimeFracional nsteps (passTime nsteps state))) pid (step + nsteps) now 0 delay
                                                          | otherwise = Manager (applyPlaysGhost (passTime nsteps state)) pid (step + nsteps) now 0 delay
                              where
                                 nsteps = (numberSteps delta delay)



-----------------BOT--------------

{--
-- | Manager we made for testing
loadOurManager :: Manager
loadOurManager = (Manager sT2 0 0 0 0 defaultDelayTime)

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step before delta delay) | odd step && odd nsteps = Manager (applyPlayBot (applyPlaysGhost (passTimeFracional nsteps (passTime nsteps state))) pid) pid (step+ nsteps) now 0 delay
                                                          | otherwise = Manager (applyPlayBot (applyPlaysGhost (passTime nsteps state)) pid) pid (step+ nsteps) now 0 delay
                              where
                                 nsteps = (numberSteps delta delay)

--}