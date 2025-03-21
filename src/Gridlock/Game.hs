{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Gridlock.Game where 

import Gridlock.DrawGrid
import Gridlock.Types 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.List
import Data.Void
import Data.Char (toUpper, toLower, isSpace)
import Control.Monad.Trans.State.Lazy 
import Control.Monad.Trans (liftIO)


type Parser = Parsec Void String
type GameStateIO = StateT GameSettings IO 


-- REUSED FROM INITIAL COURSEWORK
-- helper 1
initialiseGrid :: Int -> Int -> Map Coord Cell
initialiseGrid width height = Map.fromList [((x,y), Empty) | x <- [0..width-1], y <- [0..height-1]]


-- REUSED FROM INITIAL COURSEWORK
-- helper 2
parseInteger :: Parser Int
parseInteger = read <$> (hspace *> some digitChar <* hspace)


-- REUSED FROM INITIAL COURSEWORK
-- helper 3
toLowercase :: String -> String
toLowercase [] = []
toLowercase str = fmap toLower str


-- REUSED FROM INITIAL COURSEWORK
-- helper 4
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs


-- helper 5 
-- Checks if the string consists only of spaces, tabs etc., i.e. anything apart from letters and numbers.
isEmptyString :: String -> Bool 
isEmptyString str = all isSpace str


-- ADAPTED FROM INITIAL COURSEWORK
-- helper 6
parseToList :: Parser [String]
parseToList = do 
    items <- sepBy1 (space *> some letterChar <* space) (char ',') <* hspace -- sepBy1 (i.e. not just sepBy) is because we will need at least one "thing", otherwise we fail.
    pure items


-- REUSED FROM INITIAL COURSEWORK
-- helper 7
-- The function returns the initial state for running the monad transformer.
initialState :: GameSettings
initialState = GameSettings {gameRecord = tempGameRecord, expectedPlayer=""}
    where 
        tempGameRecord = GameRecord {players=[], grids=[], colours= Set.fromList[]}


-- ADAPTED FROM INITIAL COURSEWORK
-- Parses grid dimensions. Uses parseInteger helper function.
parseDimensions :: Parser Coord
parseDimensions = do
    width <- parseInteger -- number of columns
    char 'x'
    height <- parseInteger  -- number of rows
    pure (width, height)


-- ADAPTED FROM INITIAL COURSEWORK
parseColours :: Parser (Set Colour)
parseColours = do
    rawColours <- parseToList 
    let colours = Set.fromList $ fmap (read . capitalise) rawColours -- Since type Colour starts with an uppercase letter, we need first to capitalise the raw colours and then convert them to their corresponding types.
    pure colours


-- REUSED FROM INITIAL COURSEWORK
-- Parses coordinates which players choose to colour. This function is later utilised when parsing moves.
parseCoordinates :: Parser Coord
parseCoordinates = do
    (xCoord, yCoord) <- between (char '(') (char ')') $ do
        x <- parseInteger
        char ','
        y <- parseInteger
        pure (x,y)
    pure (xCoord,yCoord)


-- REUSED FROM INITIAL COURSEWORK
-- The function checks if any of the adjacent cells have the same colour, and if they do, the error is returned.
-- Basically, we first check if the adjacent cell we are looking for actually exists (since it could be out of bounds).
-- We then check if that cell is of the same colour as the cell relatively to which we check other cells. If it is, then the function inside "any" returns true, which then triggers error to be returned.
-- Here we need at least one True value to be returned by the function inside "any", and this already is enough to violate the rules of the game, so the error is returned.
checkAdjacentCells :: Grid -> Coord -> Colour -> Either String ()
checkAdjacentCells grid coord colour =
    if
        any
        (\x -> case x of -- Check if the cell exists
                    Nothing -> False
                    Just gridCell ->
                        case gridCell of -- Check if the cell is empty or filled with some colour
                            Empty -> False
                            (Filled cellColour) -> cellColour == colour
        )
        [Map.lookup (x,y) gridRep | (x,y) <- [(xCoord-1,yCoord), (xCoord+1,yCoord), (xCoord, yCoord-1), (xCoord, yCoord+1) ] ]
    then Left "Adjacent cell already has this colour" 
    else Right ()
    where
        xCoord = fst coord
        yCoord = snd coord
        gridRep = rep grid


-- REUSED FROM INITIAL COURSEWORK
-- The function first checks if the cell the player chose to colour is within the bounds of the board. If it is not within bounds, then we return an error, otherwise we proceed to check if this cell is already coloured. If it is coloured, then we return an error, otherwise there is the last check to carry out - that is to check whether the adjacent cells are of the same colour, and that is what we do by calling checkAdjacentCells function.
validateMove :: Grid -> Coord -> Colour -> Either String ()
validateMove grid coord colour =
    case Map.lookup coord gridRep of -- Check if the cell is within the bounds
        Nothing -> Left "Out of bounds"
        Just gridCell -> 
            case gridCell of -- Check if the cell is coloured or empty
                Filled cellColour -> Left "Cell is already coloured"
                Empty -> checkAdjacentCells grid coord colour -- Carry out further checks to see whether adjacent cells have the same colour
        where
            gridRep = rep grid


-- REUSED FROM INITIAL COURSEWORK
-- The function checks if the game is over, i.e. whether there are any other cells to colour. If there are not, then nothing is returned, otherwise an error is returned.
-- We utilise validateMove function written previously to validate each cell.
ensureGridIsFull :: Grid -> Set Colour -> Either String ()
ensureGridIsFull grid colours = 
    if (
        any (==True) $
            fmap  -- Checks for each colour
            (\colour -> (
                any 
                (\cell -> 
                    case cell of -- Tells whether the move would be valid if we were to colour this cell.
                        Right () -> True 
                        _ -> False
                )  
                [validateMove grid (x,y) colour | x <- [0..gridWidth-1], y <- [0..gridHeight-1]] 
                )  
            ) 
            (Set.toList colours)
        ) == True then Left "There are still cells available to colour!" else Right ()
    where
        gridWidth = width grid 
        gridHeight = height grid


-- The function parses player's move. In particular, it parses coordinates and the colour. It then returns the two as a pair.
-- It validates a move to ensure that the move is valid.
parseMove :: GameSettings -> Parser (Coord, Colour)
parseMove gameSettings = do 
    let GameSettings {gameRecord, expectedPlayer} = gameSettings  -- Unpack the state
    let GameRecord {players, grids, colours} = gameRecord
    let latestGrid = last grids
    coordinates <- parseCoordinates <* hspace -- Parse coordinates
    char ',' <* hspace 
    chosenColour <- (choice $ fmap (try . string . toLowercase) (fmap show (Set.toList colours))) <* space -- Parse chosen colour
    case (validateMove latestGrid coordinates (read $ capitalise chosenColour)) of -- Validate the move
        Left err -> fail err 
        Right _ -> do 
            pure (coordinates, read $ capitalise chosenColour)


-- Asks a player to enter grid dimensions. It then parses those and if there are any errors, we output "Try again" and call this function recursively.
-- If there are no errors, then we update the state.
gridDimensionsSetUp :: GameStateIO ()
gridDimensionsSetUp = do 
    liftIO $ putStrLn "Enter grid dimensions in MxN format"
    gridDimensions <- liftIO getLine

    case parse parseDimensions "" gridDimensions of 
        Right (gridWidth, gridHeight) -> do 
            let initialGrid = Grid {width= gridWidth , height = gridHeight, rep = initialiseGrid gridWidth gridHeight }
            let initialGameRecord = GameRecord {players=[], grids=[initialGrid], colours= Set.fromList[]}
            let initialGameSettings = GameSettings {gameRecord=initialGameRecord, expectedPlayer=""}
            put initialGameSettings -- Load the state for the first time.
        Left err -> do 
            liftIO $ putStrLn "Incorrect format. Try again"
            gridDimensionsSetUp
        

-- Asks players to enter their names. It then checks to see if any of the names is an empty string, and if they are, then we print "Try again" and call the function recursively.
-- If there are no errors, then the state is updated.
namesSetUp :: GameStateIO ()
namesSetUp = do 
    liftIO $ putStrLn "Enter the name of the 1st player"
    firstPlayer <- liftIO getLine 
    liftIO $ putStrLn "Enter the name of the 2nd player"
    secondPlayer <- liftIO getLine 
    if (isEmptyString firstPlayer || isEmptyString secondPlayer) then 
        do 
            liftIO $ putStrLn "Name can not be empty. Try again"
            namesSetUp
    else 
        do
            gameSettings <- get -- Fetch the state
            let GameSettings {gameRecord, expectedPlayer} = gameSettings
            let updatedGameRecord = gameRecord {players=[firstPlayer, secondPlayer]} -- Put the names of the players in the state.
            let updatedGameSettings = gameSettings {gameRecord=updatedGameRecord,expectedPlayer=firstPlayer}
            put updatedGameSettings -- Update the state


-- Asks players to enter colours they want to play. It then parses those and updates the state if there are no errors.
-- If there are any errors, then we ask to enter colours again.
coloursSetUp :: GameStateIO ()
coloursSetUp = do 
    liftIO $ putStrLn ("Enter any of the following colours you want to play: " ++  (intercalate (", ") $ map (toLowercase . show) ([minBound..maxBound] :: [Colour])) ++ " in colour1, colour2, ... colourN format." )
    chosenColours <- liftIO getLine
    case parse parseColours "" chosenColours of 
        Right (parsedColours) -> do 
            gameSettings <- get -- Fetch the state
            let GameSettings {gameRecord, expectedPlayer} = gameSettings
            let updatedGameRecord = gameRecord {colours=parsedColours} -- Put the chosen colours in the state
            let updatedGameSettings = gameSettings {gameRecord=updatedGameRecord}
            put updatedGameSettings -- Update the state
        Left err -> do 
            liftIO $ putStrLn "Incorrect format. Try again"
            coloursSetUp


-- The function processes a move, i.e. it first checks whether the game is over or not. If it is, then we print the name of the winner and end the game.
-- Otherwise we ask a player to enter coordinates and the colour. We then parse that input using parseMove function. 
-- If there are no errors, then we update the game state and call processMove again for the next player to make its move. If there are errors, then we ask a player to try again and call processMove again.
processMove :: GameStateIO ()
processMove = do 
    currentState <- get -- Fetch the state
    let GameSettings {gameRecord, expectedPlayer} = currentState
    let GameRecord {players, grids, colours} = gameRecord
    let latestGrid = last grids
    let currentPlayer = expectedPlayer -- Just for better readability
    let prevPlayer = concat $ filter (/= currentPlayer) players

    case (ensureGridIsFull latestGrid colours) of -- Check if the game has ended already before asking for another move.
        Right _ -> liftIO $ putStrLn ("Game is over.\nPlayer " ++ prevPlayer ++ " has won.")
        Left _ -> do 
            liftIO $ putStrLn (currentPlayer++ " , please enter the coordinates of the cell you want to colour and the colour itself in (x,y),colour format, e.g. (1,1),red ")
            playerMove <- liftIO getLine 
            let parseResult = parse (parseMove currentState) "" playerMove -- Parse the move
            case parseResult of 
                Left err -> do 
                    liftIO $ putStrLn $ (errorBundlePretty err) ++ "\nTry again !" -- If parsing was unsuccessful, then display an error.
                Right (coordinates, colour) -> do 
                    let updatedGrid = latestGrid {rep = Map.insert coordinates (Filled colour) (rep latestGrid) }
                    let updatedGameRecord = gameRecord {grids= grids ++ [updatedGrid]}
                    put (GameSettings {gameRecord=updatedGameRecord, expectedPlayer=prevPlayer}) -- If the parsing was successful, then update the state.
                    liftIO $ putStrLn (drawGrid updatedGrid) -- Display the grid.
            processMove -- Call the function again to process the next move.


-- The function asks a player if they want to play a game. If they do, then the game begins by listing the rules.
-- We then call some of the functions written earlier.
playGame :: GameStateIO ()
playGame = do 
    liftIO $ putStrLn "Do you want to play a Gridlock game ? (Y\\N) "
    playerDecision <- liftIO getLine 
    if playerDecision == "Y" then 
        do
            liftIO $ putStrLn "Ok, here are the rules: \n 1. The game is played on a grid M by N cells.\n 2. On your turn, you may colour in any cell of the grid, so long as you do not colour it the same colour as an adjacent cell.\n 3. The game ends when no more moves can be made, and the player who made the last move wins.\n 5. Good Luck !"
            gridDimensionsSetUp
            coloursSetUp
            namesSetUp
            processMove
    else 
        liftIO $ putStrLn "Goodbye then!"