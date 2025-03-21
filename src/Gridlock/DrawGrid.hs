module Gridlock.DrawGrid where

import Gridlock.ColourSquares (square)
import Gridlock.Types

-- Hint: These might come in handy :)
import Data.Map (Map)
import Data.Map qualified as Map


-- REUSED FROM INITIAL COURSEWORK
-- We draw a grid by drawing each individual raw and then concatMap them.
drawGrid :: Grid -> String
drawGrid grid = borders ++ "\n" ++ concatMap (createRow grid) [0..(numOfRows-1)] ++ borders
    where
        numOfColumns = width grid
        numOfRows = height grid
        borders = "+" ++ replicate numOfColumns '-' ++ "+"

-- REUSED FROM INITIAL COURSEWORK
-- Outputs a string representation of each raw of the grid.
-- For each cell in the current raw we call getColour function and concatMap the result.
createRow :: Grid -> Int -> String 
createRow grid y = "|" ++  concatMap (\x -> getColour gridRep x y) [0..(numOfColumns-1)] ++ "|\n"
    where
        gridRep = rep grid
        numOfColumns = width grid

-- REUSED FROM INITIAL COURSEWORK
-- Outputs a string representation of a colour or an empty string with a space if the cell is not coloured.
getColour :: Map Coord Cell -> Int -> Int -> String
getColour gridRep x y = case cell of
    Just Empty -> " "
    Just (Filled colour) -> square colour
    Nothing -> ""
    where 
        cell = Map.lookup (x,y) gridRep