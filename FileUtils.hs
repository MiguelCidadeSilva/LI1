-- | This module was given to us by the professors, we did not use anything in here, but we added this file just in case 
module FileUtils where

import System.IO.Unsafe
import Types

loadMaze :: String -> State
loadMaze filepath = unsafePerformIO $ readStateFromFile filepath


readStateFromFile :: String -> IO State
readStateFromFile f = do
                content <- readFile f
                let llines = lines content 
                    (new_map,pl) = convertLinesToPiece llines 0 []
                return (State new_map pl 1)



convertLinesToPiece :: [String] -> Int -> [Player] -> (Maze,[Player])
convertLinesToPiece [] _ l = ([],l)
convertLinesToPiece (x:xs) n l  = let (a,b) = convertLineToPiece x n 0 l 
                                      (a1,b1) = convertLinesToPiece xs (n+1) b
                                  in (a:a1,b1)

convertLineToPiece :: String -> Int -> Int -> [Player] -> ([Piece],[Player])
convertLineToPiece [] _ _ l = ([],l)
convertLineToPiece (z:zs) x y l = let (a,b ) = charToPiece z x y l
                                      (a1,b1) = convertLineToPiece zs x (y+1) b 
                                  in (a:a1,b1)


charToPiece :: Char -> Int -> Int -> [Player] -> (Piece,[Player])
charToPiece c x y l
    | c == '{' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Normal ))   :l) )
    | c == '<' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Closed Normal )) :l) )
    | c == '}' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Open Normal ))   :l) )
    | c == '>' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Closed Normal )) :l) )
    | c == 'V' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Open Normal ))   :l) )
    | c == 'v' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Closed Normal )) :l) )
    | c == '^' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Open Normal ))   :l) )
    | c == '|' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Closed Normal )) :l) )
    | c == 'X' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Dying ))    :l) )
    | c == 'M' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Alive))            :l) )
    | c == '?' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Dead))             :l) )
    | c == 'o' =  (Food Big,l)
    | c == '.' =  (Food Little,l)
    | c == '#' =  (Wall,l)
    | otherwise = (Empty,l)