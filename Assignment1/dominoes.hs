module Dominoes where

  type Domino = [(Int, Int)]
  type Hand = [(Domino)]
  type Board = [(Domino)]
  type End = [(Char)]

goesP :: Domino -> Board -> End ->Bool
