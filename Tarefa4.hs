{- |

= Introduction of the Task

This is the first task of the second phase.
On this task we had to create the function passTime that moves every player based on their orientation.

= Objectives of the task:

1- Every player on the list of players must move after a certain number of interactions.

2- Apply the main function of Tarefa2 (play function) to every player.

3- Change the Mouth of the Pacmans when the number of interactions is odd.

4- Change the PacMode and the GhostMode of every player on the list of players when the time of Mega ends,
   if the time of PacMode Mega does not end, the time of Mega must be reduced, based on the number of 
   interactions.

= How we solved these issues?

1- We made two functions megaEnd and megaEndPlayers that given the number of steps check if the time of
   PacMode Mega will end after ocurring all the given steps.

2- We started working on the main function of the task, passTime, and made it so, that if the number of 
   interactions is higher or equal to 2 it will evoke itself recursively but the number of steps will be 
   reduced twice. In this case, it will check if in the middle of the two interactions, the time of Pacmode
   Mega will end using the function megaEnd with two steps.

2.1- In the case where megaEnd returns True we created a function named changeModes that changes the
     player Modes.

2.2- On the other cases it will reduce the time of Mega two times.

3- When the number of interactions is equal to 1 we made passTime do either:

3.1- In the case where megaEnd returns True we created a function named changeModes that changes the
     player Modes, and it will also change the Mouth of the Pacmans by evoking a function that is called
     changeMouthState.

3.2- On the other cases it will reduce the time of Mega once and it will also change the Mouth of the
     Pacmans through changeMouthState.

4- The function passTime after doing the points 2 and 3 will evoke the function playPasstime by giving the
   state after the changes, the minimum ID of the list of players and the number 1 or 2, it will depend like
   we mentioned before if the number of interactions is higher than or equal to 2 or equal to 1.

5- We created a function named playPassTime that will move every player one or two steps, given by the main
   function of the task.

5.1 - If the number of steps is two it will make two times the velocity the play for the ID given, using the
      function playApply that applies n times the play function defined on Tarefa2. After this playPassTime
      will evoke itself recursively by adding 1 to the ID, maintaining the number of steps, 2 on this case.

5.2 - When the number of steps is one it will do the same thing as when the number of steps is 2, but it will
      not apply the play for players with fractional velocity, and the number of times that it will make the
      play for players with Int velocity will be the same number as the velocity .

5.3 - When the ID given is higher than the maximum ID of the player list, the function will give the same
      State, as the one given.

6- When the number of steps is 0 it will give the same State as the one given.

7- We created auxiliary functions throughout the task to give us some information about specific player
   attriutes of a player with a given ID on a player list. 

8- We added to the code the function passTimeFractional that will be called on Main in a specific case and
   it does the same thing as passtime, but for players with fractional velocities.

= Discussion and Conclusion

This task was a bit confusing but when we understood the instructions, it became clear and we managed to
complete the task without many issues.
We could have done this task with simpler code, but in the end we completed our established goals so we 
decided to advance on the project instead of optimizing the code to save time.
We enjoyed this task since it let us play our game for the first time in the terminal.

-}

module Tarefa4 where
import Types
import Tarefa2
import Tarefa5
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import FileUtils


teste1T4 :: State
teste1T4 = passTime 5 sT2

teste2T4 :: State
teste2T4 = passTime 4 sT3

teste3T4 :: State
teste3T4 = passTime 10 sT4

-- | Default delay time
defaultDelayTime = 250 -- 250 ms

-- | Main function of the task
passTime :: Int -> State -> State
passTime 0  s = s
passTime st s | st >= 2 && megaEnd s 2 = passTime (st-2) $ playPassTime (changeModes s) i 2
              | st >= 2                = passTime (st-2) $ playPassTime (reduceTimeMega $ reduceTimeMega s) i 2
              | megaEnd s 1            = playPassTime (changeMouthState $ changeModes s) i 1
              | otherwise              = playPassTime (changeMouthState $ reduceTimeMega  s) i 1
          where
            i  = minIDState s 

-- | Makes the movement for all players (Pacmans and Ghosts) on the State
playPassTime :: State -> Int -> Int -> State
playPassTime (State maze [] le) i _ = State maze [] le
playPassTime (State maze lp le) i 1 | i <= maxID lp 0 && intElemList v generateIntList = playPassTime (playApply (State maze lp le) v i) (i+1) 1
                                    | i <= maxID lp 0 = playPassTime (State maze lp le) (i+1) 1
                                    | otherwise       = (State maze lp le)
                    where           v = getVelocity lp i  
playPassTime (State maze lp le) i 2 | i <= maxID lp 0 = playPassTime (playApply (State maze lp le) (2*v) i) (i+1) 2
                                    | otherwise       = State maze lp le
                    where           v = getVelocity lp i

-- | Calculates how many times we need to apply the function play from Tarefa 2
playApply :: State -> Double -> Int -> State 
playApply s 0  _  = s    
playApply (State maze lp le) nv id = playApply (play (Move id (getIdOrientation lp id)) (State maze lp le)) (nv-1) id

-- | Gets the highest ID on a list of players
maxID :: [Player] -> Int -> Int
maxID [] id = id
maxID (Pacman (PacState (id,(_,_),_,_,_,_) _ _ _):xs) i | id > i    = maxID xs id
                                                        | otherwise = maxID xs i
maxID (Ghost  (GhoState (id,(_,_),_,_,_,_) _):xs) i | id > i    = maxID xs id
                                                    | otherwise = maxID xs i

-- | Gets the value of the velocity on the state of one player when given the ID of that player 
getVelocityState :: State -> Int -> Double
getVelocityState (State m lp le) id = getVelocity lp id

-- | Gets the value of the velocity of a Player with a given ID on a list of Players 
getVelocity :: [Player] -> Int -> Double
getVelocity [] _ = 0
getVelocity (Pacman (PacState (id,(_,_),v,_,_,_) _ _ _):xs) i | i == id   = v
                                                              | otherwise = getVelocity xs i
getVelocity (Ghost  (GhoState (id,(_,_),v,_,_,_) _):xs) i | i == id   = v
                                                          | otherwise = getVelocity xs i

-- | Gets the value of the orientation of a Player with a given ID on a list of Players
getIdOrientation :: [Player] -> Int -> Orientation
getIdOrientation [] _ = Null
getIdOrientation (Pacman (PacState (id,(_,_),_,o,_,_) _ _ _):xs) i | i == id = o
                                                                   | otherwise = getIdOrientation xs i
getIdOrientation (Ghost  (GhoState (id,(_,_),_,o,_,_) _):xs) i | i == id = o
                                                               | otherwise = getIdOrientation xs i

-- | Gets the orientation on the State of one player given its own id 
getIdOState :: State -> Int -> Orientation
getIdOState (State maze lp le) id = getIdOrientation lp id

-- | Checks if PacMode Mega is going to end given a state and the number of steps
megaEnd :: State -> Int -> Bool
megaEnd (State _ pl _) st = megaEndPlayers pl st

-- | Verifies if a Pacman is ending Mega mode given and the number of steps 
megaEndPlayers :: [Player] -> Int -> Bool
megaEndPlayers [] _ = False
megaEndPlayers (Pacman (PacState _ t _ pm):xs) st | pm == Mega && t <= std = True
                                                  | otherwise  = megaEndPlayers xs st
                              where std = fromIntegral st
megaEndPlayers (x:xs) st = megaEndPlayers xs st

-- | Change the mouth of the pacmans in the state  
changeMouthState :: State -> State
changeMouthState (State m pl l) = State m (changeMouth pl) l

-- | Change the mouth of every Pacman  
changeMouth :: [Player] -> [Player]
changeMouth [] = []
changeMouth (Pacman (PacState p t Open pm):ps)   = Pacman (PacState p t Closed pm) : changeMouth ps
changeMouth (Pacman (PacState p t Closed pm):ps) = Pacman (PacState p t Open pm) : changeMouth ps
changeMouth (p:ps) = (p : changeMouth ps)

-- | Changes the modes of the Ghosts and the Pacmans when the PacMode Mega ends
changeModes :: State -> State
changeModes (State m pl l) = State m (changePacMode (changeGhostAlive pl)) l

-- | Reduce the time of PacMode mega
reduceTimeMega :: State -> State
reduceTimeMega (State m pl l) = State m (map reduceTimeMegaPlayer pl) l

-- | Changes the Pacmode from Mega to Normal when the time of PacMode Mega ends
changePacMode :: [Player] -> [Player]
changePacMode [] = []
changePacMode (Pacman (PacState p t mm Mega):xs) = Pacman (PacState p 0 mm Normal)   : changePacMode xs
changePacMode (x:xs) = x : changePacMode xs 

-- | Auxiliary function of reduceTimeMega
reduceTimeMegaPlayer :: Player -> Player
reduceTimeMegaPlayer (Pacman (PacState p t mm Mega)) = Pacman (PacState p (t-0.25) mm Mega) 
reduceTimeMegaPlayer x = x 

-- | Changes the ghostMode from Dead to Alive when Pacman has ended is time of PacMode Mega
changeGhostAlive :: [Player] -> [Player]
changeGhostAlive [] = []
changeGhostAlive (Ghost(GhoState (id,(x,y),v,o,p,l) Dead) :xs) = (Ghost (GhoState (id,(x,y),v*2,o,p,l) Alive)) :  changeGhostAlive xs   
changeGhostAlive (p:ps) = p : changeGhostAlive ps

-- | Gets the minimum ID of the players on a State
minIDState :: State -> Int
minIDState (State m lp le) = minID lp (maxID lp 0)

-- | Gets the minimum ID of the list of players
minID :: [Player] -> Int -> Int
minID [] id = id
minID (Pacman (PacState (id,(_,_),_,_,_,_) _ _ _):xs) i | id <= i = minID xs id
                                                        | otherwise = minID xs i
minID (Ghost  (GhoState (id,(_,_),_,_,_,_) _):xs) i | id <= i = minID xs id
                                                    | otherwise = minID xs i

-- | Generates a list with every integer
generateIntList :: [Integer]
generateIntList = [0 ..]

-- | Checks if the velocity of the player is an integer
intElemList :: Double -> [Integer] -> Bool
intElemList _ [] = False
intElemList n (x:xs) | n == xin = True
                     | n < xin = False
                     | otherwise = intElemList n xs
    where xin = fromIntegral x


-- | This is the passTime funtion for players with fracional velocity  
passTimeFracional :: Int -> State -> State
passTimeFracional n s@(State m lp le) = playTimeFracional n s 0 (maxID lp 0)

-- | Applies the play for every player that has fracional velocity
playTimeFracional :: Int -> State -> Int -> Int -> State
playTimeFracional n s@(State maze lp@(Ghost (GhoState (id,(x,y),v,o,p,l) gm):xs) le) i idm | i > idm = s
                                                                                           | intElemList v generateIntList && i == id = playTimeFracional n s (i+1) idm
                                                                                           | i == id = playTimeFracional n (playApply s nv i) (i+1) idm
                                                                                           | otherwise =  playTimeFracional n (State maze (xs ++ [Ghost (GhoState (id,(x,y),v,o,p,l) gm)]) le) i idm
                                                                            where
                                                                              nv = fromIntegral n

playTimeFracional n s@(State maze lp@(Pacman (PacState (id,(x,y),v,o,p,l) t m pm) :xs) le) i idm | i > idm = s
                                                                                                 | intElemList v generateIntList && i == id = playTimeFracional n s (i+1) idm
                                                                                                 | i == id = playTimeFracional n (playApply s nv i) (i+1) idm
                                                                                                 | otherwise =  playTimeFracional n (State maze (xs ++ [Pacman (PacState (id,(x,y),v,o,p,l) t m pm)]) le) i idm 
                                                                                      where
                                                                                        nv = fromIntegral n                                              