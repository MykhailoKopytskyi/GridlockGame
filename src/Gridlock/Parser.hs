module Gridlock.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Void
import Gridlock.DrawGrid
import Gridlock.Types

type Parser = Parsec Void String

{- | Parse a game into a GameRecord.

To run this function (either as part of your application, or for testing, you can use one of the parse* functions from the Megaparsec library, such as "parse" or "parseTest".)
-}
parseGame :: Parser GameRecord
parseGame = error "Not implemented"
