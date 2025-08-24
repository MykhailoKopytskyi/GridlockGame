# Gridlock game parser

This program was created as part of the 2nd coursework for the Functional Programming module that I took at the university. The program is written in Haskell programming language.

## Table of contents
- [Overview](#overview)
- [Rules of the game](<Rules of the game>)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)



## Overview
When undertaking the Functional Programming module, we completed two courseworks. This is the second (optional) part of the 2nd coursework. I decided to extend the first part of the coursework and create a fully playable game of Gridlock. Two players can play against each other.

## Rules of the game 
Gridlock is a simple colouring game played between two players. Here are the rules:
- The game is played on a grid made of M by N cells. The grid is initially empty.
- On a player’s turn, they may colour in any cell of the grid, so long as they do not
colour it the same colour as an adjacent cell.
- The game ends when no more moves can be made, and the player who made the
last move wins.




## Features 
- Players can choose their username, colours, grid dimensions etc.
- Two players can play a game by colouring one of the cells with a specific colour of their choice.
- The game continues until no more cells can be coloured.




## Prerequisites
- The GHC
- cabal-install
- Stack   

See [this](https://www.haskell.org/downloads/) website for further details.

## Installation
Use ``git clone`` or download the .zip file.

## Usage 
Navigate to the installed folder and run ``stack repl`` and then call the ``main``
method to play the game between yourself and another person.

