-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid
import Gridlock.Game
import Gridlock.Types
import System.Environment
import Text.Megaparsec

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.State.Lazy 



-- This is the extension of the initial coursework.
-- I have implemented the program which allows two users to play a Gridlock game.
-- I have reused some of the functions from the initial coursework. I will annotate which functions have been reused.
-- When you run the main method, the program will ask you if you want to play a game, and if you do, then it lists all the rules, and asks to enter grid dimensions.
-- It then asks to enter colours and players names and then the game begins.
-- After each move, the updated grid is drawn.
-- When there are no moves left to play, the game ends and outputs the winner.


main :: IO ()
main = do 
    runStateT playGame initialState -- Runs the game with initialState
    pure ()