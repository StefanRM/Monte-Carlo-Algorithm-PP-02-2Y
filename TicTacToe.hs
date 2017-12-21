{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, TupleSections #-}

module TicTacToe where

import MCTS
import GameState

import System.Random
import Data.List

{-
    Tipul celulelor (1-9)
-}
type Cell = Int

{-
    Tipul jucătorilor
-}
data Player = X | O
    deriving (Eq, Enum, Show)

{-
    Întoarce celălalt jucător.
-}
otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

{-
    *** TODO ***

    Tipul stării jocului. Ar trebui să conțină informații despre tablă
    și despre jucătorul care urmează să mute.
-}
data Board = B {
    cells :: [[(Cell, Maybe Player)]],
    player :: Player
}
    deriving Eq

{-
    *** TODO ***

    Întoarce lista conținuturilor celulelor, unde celule libere
    sunt reprezentate de `Nothing`.

    Ordinea celulelor este următoarea:

    789
    456
    123
-}
boardConfiguration :: Board -> [Maybe Player]
boardConfiguration (B cells player) = foldl (\accRow row -> accRow ++ (foldl (\accCell (cellNr, cell) -> accCell ++ [cell]) [] row)) [] cells 

{-
    *** TODO ***

    Întoarce jucătorul care urmează să mute.
-}
boardPlayer :: Board -> Player
boardPlayer (B cells player) = player

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Board`.
-}
instance Show Board where
    show (B cells player) = foldl (\accRow row -> accRow ++ (foldl (\accCell (cellNr, cell) -> accCell ++ show cellNr ++ (case cell of
                                                                                                    Nothing -> " _ "
                                                                                                    Just x -> " " ++ show x ++ " ")) "" row) ++ "\n") "" cells 

{-
    *** TODO ***

    Instanțiați clasa `GameBoard` cu tipurile `Board` și `Cell`.
-}
instance GameState Board Cell where
    -- playerIndex :: Board -> Int
    playerIndex (B cells player) = case player of
                                    O -> 0
                                    X -> 1

    -- maxPlayers :: Board -> Int
    maxPlayers (B cells player) = 2

    -- successors :: Board -> [(Cell, Board)]
    successors board = foldl (\acc cell -> case (place cell board) of
                                            Nothing -> acc
                                            Just x -> acc ++ [(cell, x)]) [] [1..9]

    -- outcome :: Board -> Outcome
    outcome (B cells player) = if (isWinHorizon (B cells player) X) || (isWinHorizon (B cells player) O) || (isWinVert (B cells player) X) || (                         isWinVert (B cells player) O) || (isWinDiag (B cells player) X) || (isWinDiag (B cells player) O) then
                                    Win 1
                                else
                                    if isOngoing (B cells player) then
                                        Ongoing
                                    else
                                        Draw 0

isOngoing (B cells player) = foldl (\accRow row -> accRow || (foldl (\accCell (cellNr, cell) -> accCell || (cell == Nothing)) False row)) False cells
isWinHorizon (B cells player) playerCheck = foldl (\accRow row -> accRow || (foldl (\accCell (cellNr, cell) -> accCell && (cell == Just playerCheck)) True row)) False cells
isWinVert (B cells player) playerCheck = isWinHorizon (B (transpose cells) player) playerCheck
isWinDiag (B cells player) playerCheck = if (snd ((cells !! 0) !! 0) == Just playerCheck) && (snd ((cells !! 1) !! 1) == Just playerCheck) && (snd                                      ((cells !! 2) !! 2) == Just playerCheck) then
                                            True
                                          else
                                            if (snd ((cells !! 0) !! 2) == Just playerCheck) && (snd ((cells !! 1) !! 1) == Just playerCheck) && (snd ((cells !! 2) !! 0) == Just playerCheck) then
                                                True
                                            else
                                                False

{-
    *** TODO ***

    Tabla inițială de joc. X mută primul.
-}
initialBoard :: Board
initialBoard = (B [[(1, Nothing), (2, Nothing), (3, Nothing)], [(5, Nothing), (4, Nothing), (6, Nothing)], [(7, Nothing), (8, Nothing), (9, Nothing)]] X)

{-
    *** TODO ***

    Mută în celula dată ca parametru, în funcție de jucătorul aflat la rând,
    și schimbă jucătorul curent.

    Ordinea celulelor este explicată la funcția `boardConfiguration`.
-}
place :: Cell -> Board -> Maybe Board
place cellPlace (B cells player) = if (foldl (\res row -> res && (foldl (\res2 (cellNr, cell, valBool) -> res2 && valBool) True row)) True                                  puttingCell) == False then
                                        Nothing
                                    else
                                        if (cellPlace < 1 || cellPlace > 9) then
                                            Nothing
                                         else
                                            Just (B (foldl (\res row -> res ++ [(foldl (\res2 (cellNr, cell, valBool) -> res2 ++ [(cellNr, cell)]) [] row)]) [] puttingCell) (otherPlayer player))
    where 
        puttingCell = foldl (\accRow row -> accRow ++ [(foldl (\accCell (cellNr, cell) -> accCell ++ [if cellNr == cellPlace then
                                                                                                        case cell of
                                                                                                            Nothing -> (cellNr, Just player, True)
                                                                                                            x -> (cellNr, x, False)
                                                                                                      else
                                                                                                        (cellNr, cell, True)]) [] row)]) [] cells

{-
    *** TODO ***

    Alege o mutare pornind din starea curentă.

    Utilizați `choose` din modulul `MCTS`, cu un număr dorit de iterații
    ale algoritmului.

    Pentru a juca contra calculatorului, rulați din modulul `Interactive`:

    > humanVsAI step
-}
step :: Board -> StdGen -> (Cell, Board)
step = undefined
