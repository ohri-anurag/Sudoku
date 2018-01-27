module Sudoku(
    solve,
    fromList,
    Sudoku,
    value,
    get
    ) where

import Data.Maybe(fromJust)
import qualified Data.Map as M
import Data.List(foldl', sortBy, sort)


unique :: (Ord a) => [a] -> [a]
unique xs = helper [] (sort xs)
    where
        helper acc [] = reverse acc
        helper acc (x:xs) = helper (x:acc) $ dropWhile (== x) xs

intersection :: (Ord a) => [a] -> [a] -> [a]
intersection as bs
    | null as || null bs = []
    | a == b = a : intersection (tail as) (tail bs)
    | a <  b = intersection (tail as) bs
    | otherwise = intersection as (tail bs)
    where
        a = head as
        b = head bs


-- A candidate represents a possible value that could fill a cell.
type Candidate = Int
-- A value represents the actual value of the cell.
type Value = Int

-- A cell represents one square out of the 81 (9x9) squares present in the sudoku.
-- It contains a value, and a candidate list.
-- Two cases:
--      Value = 0, Candidate List = [...] (In this case candidate list can't be empty, a 0 value represents an unfilled cell)
--      Value = n, Candidate List = [] , 1 <= n <= 9 (In this case, the cell has already been filled, and the candidate list must be empty)
data Cell = Cell {
    value :: Value,
    candidates :: [Candidate]
    } deriving (Eq, Show)

-- A position represents the (row,column) pair for a cell.
type Position = (Int,Int)

-- A sudoku is a map from Position -> Cell
type Sudoku = M.Map Position Cell

data SudokuState = Solved | Illegal | Unsolved
    deriving (Eq,Show)

-- Create a Sudoku from a list
fromList :: [[Int]] -> Sudoku
fromList xss = fillCandidates $ M.fromDistinctAscList xs
    where
        xs = zip positions (map (flip Cell []) $ concat xss)

-- Fill the candidates in a newly created Sudoku
fillCandidates :: Sudoku -> Sudoku
fillCandidates sudoku = foldl' fill sudoku positions
    where
        fill sudoku pos = if val == 0
                            then M.insert pos (Cell val $ complement $ unique $ map (value . flip get sudoku) $ getRBC pos ((/=) 0) sudoku) sudoku
                            else sudoku
            where
                val = value $ get pos sudoku

check :: Sudoku -> SudokuState
check sudoku = helper (M.toList sudoku)
    where
        helper [] = Solved
        helper ((_,(Cell v pl)):xs) =
            if v == 0 && null pl
                then Illegal
                else if v == 0
                    then Unsolved
                    else helper xs

-- A list of all the possible positions in a 9x9 Sudoku
positions :: [Position]
positions = [(i,j) | i <- [1..9], j <- [1..9]]

-- Get a cell at the specified position
get :: Position -> Sudoku -> Cell
get pos sudoku = fromJust $ M.lookup pos sudoku

-- Get the complete row in which the cell at the given position lies, except the given cell.
getRow :: Position -> (Value -> Bool) -> Sudoku -> [Position]
getRow (i,j) pred sudoku = [(i,c) | c <- [1..9], c /= j, let v = value $ get (i,c) sudoku, pred v]

-- Get the complete column in which the cell at the given position lies, except the given cell.
getCol :: Position -> (Value -> Bool) -> Sudoku -> [Position]
getCol (i,j) pred sudoku = [(r,j) | r <- [1..9], r /= i, let v = value $ get (r,j) sudoku, pred v]

-- Get the complete box in which the cell at the given position lies, except the given cell.
getBox :: Position -> (Value -> Bool) -> Sudoku -> [Position]
getBox (i,j) pred sudoku = [(i'',j'') | r <- [1..3], c <- [1..3], let i'' = i'+r, let j'' = j'+c, i /= i'' || j /= j'', let v = value $ get (i'',j'') sudoku, pred v]
    where
        i' = 3 * div (i-1) 3
        j' = 3 * div (j-1) 3

-- Get the cells that lie in the row, column and box in which the cell at the given position lies, except the given cell.
getRBC :: Position -> (Value -> Bool) -> Sudoku -> [Position]
getRBC pos pred sudoku = unique $ row ++ col ++ box
    where
        row = getRow pos pred sudoku
        col = getCol pos pred sudoku
        box = getBox pos pred sudoku

-- Complement the candidate list, with [1,2,3,4,5,6,7,8,9] acting as the universal set.
complement :: [Candidate] -> [Candidate]
complement cs = filter (flip notElem cs) [1..9]

-- Remove a candidate from all the cells that lie in the row, column and box of the given cell.
removeCandidate :: Position -> Candidate -> Sudoku -> Sudoku
removeCandidate pos c sudoku = foldl' (remove [c]) sudoku $ getRBC pos ((==) 0) sudoku

-- Remove the given candidates from the given cell.
remove :: [Candidate] -> Sudoku -> Position -> Sudoku
remove cs sudoku pos = helper (get pos sudoku)
    where
        helper (Cell v pList) = M.insert pos (Cell v (filter (flip notElem cs) pList)) sudoku

-- Other helper functions
rm :: [Position] -> Sudoku -> (Position,[Candidate]) -> Sudoku
rm ps sudoku (p,pList) = foldl' (remove pList) sudoku $ filter (/= p) ps

common :: [Candidate] -> Sudoku -> Position -> (Position,[Candidate])
common cs sudoku pos' = (,) pos' $ intersection cs (candidates $ get pos' sudoku)

uncommon :: [Position] -> Sudoku -> (Position,[Candidate]) -> (Position,[Candidate])
uncommon ps sudoku (p,list) = (,) p $ intersection list $ complement $ unique $ concatMap (candidates . flip get sudoku) $ filter (/= p) ps


-- Sudoku solver functions
nakedSingle :: Sudoku -> Sudoku
nakedSingle sudoku = foldl' check sudoku positions
    where
        check sudoku pos = helper (get pos sudoku)
            where
                helper (Cell 0 [p]) = removeCandidate pos p $ M.insert pos (Cell p []) sudoku
                helper _ = sudoku

nakedPair :: Sudoku -> Sudoku
nakedPair sudoku = foldl' (flip ($)) sudoku [helper getRow , helper getCol, helper getBox]
    where
        helper fn sudoku = foldl' check sudoku positions
            where
                check sudoku pos = if length cs == 2 && length pairElems == 1
                                    then rm xs sudoku (h,cs) -- foldl' (remove cs) sudoku $ filter (/= h) xs
                                    else sudoku
                    where
                        cs = candidates $ get pos sudoku
                        xs = fn pos ((==) 0) sudoku
                        pairElems = filter ((==) cs . candidates . flip get sudoku) xs
                        h = head pairElems

hiddenSingle :: Sudoku -> Sudoku
hiddenSingle sudoku = foldl' check sudoku positions
    where
        check sudoku pos = if not (null list)
                            then removeCandidate pos h $ M.insert pos (Cell h []) sudoku
                            else sudoku
            where
                h = head list
                list = concat $ filter ((==) 1 . length) $ map (intersection cs . complement . unique . concatMap (candidates . flip get sudoku)) [row,col,box]
                cs = candidates $ get pos sudoku
                row = getRow pos ((==) 0) sudoku
                col = getCol pos ((==) 0) sudoku
                box = getBox pos ((==) 0) sudoku

hiddenPair :: Sudoku -> Sudoku
hiddenPair sudoku = foldl' (flip ($)) sudoku [helper getRow, helper getCol, helper getBox]
    where
        helper fn sudoku = foldl' check sudoku positions
            where
                check sudoku pos = if not (null list)
                                    then foldl' f sudoku list
                                    else sudoku
                    where
                        cell = get pos sudoku
                        cs = candidates cell
                        xs = fn pos ((==) 0) sudoku
                        list = filter ((==) 2 . length . snd) $ map (uncommon xs sudoku . common cs sudoku) xs
                        f sudoku (p,ps) = M.insert pos (Cell (value cell) ps) $ M.insert p (Cell (value $ get p sudoku) ps) sudoku

pointingPair :: Sudoku -> Sudoku
pointingPair sudoku = foldl' (flip ($)) sudoku [helper getRow getBox, helper getBox getRow, helper getCol getBox, helper getBox getCol]
    where
        helper f1 f2 sudoku = foldl' check sudoku positions
            where
                check :: Sudoku -> Position -> Sudoku
                check sudoku pos = if null cs
                                    then sudoku
                                    else foldl' (rm as) sudoku asbs
                    where
                        cs = candidates $ get pos sudoku

                        as = f1 pos ((==) 0) sudoku
                        bs = f2 pos ((==) 0) sudoku

                        asbs :: [(Position,[Candidate])]
                        asbs = filter ((==) 1 . length . snd) $ map (uncommon bs sudoku . common cs sudoku) $ intersection as bs


methods :: Sudoku -> Sudoku
methods sudoku = foldl' (flip ($)) sudoku [hiddenPair, nakedPair, pointingPair, hiddenSingle, nakedSingle]

-- Uses the Sudoku solver functions to reduce the candidates for the given sudoku.
-- If a solution is found, returns it.
-- Otherwise, returns when it can't reduce any further candidates.
reduce :: Sudoku -> Sudoku
reduce sudoku
    | state == Solved ||
      state == Illegal ||
      sudoku == sudoku' = sudoku
    | otherwise    = reduce sudoku'
    where
        sudoku' = methods sudoku
        state = check sudoku

-- Uses reduce to solve the Sudoku after making guesses for the cell with the least number of candidates.
guess :: Sudoku -> Maybe Sudoku
guess sudoku
    | state == Solved = Just sudoku'
    | state == Unsolved = helper myGuesses
    | state == Illegal = Nothing
    where
        sudoku' = reduce sudoku

        state = check sudoku'

        helper [] = Nothing
        helper (g:gs) = case guess g of
            Just g' -> Just g'
            Nothing -> helper gs

        myGuesses = map f pList

        f p = removeCandidate pos p $ M.insert pos (Cell p []) sudoku'

        (pos, (Cell v pList)) = head $ sortBy (\(p1,c1) (p2,c2) -> compare (length $ candidates c1) (length $ candidates c2)) $ 
                                    filter (\(p,c) -> not $ null $ candidates c) $ M.toList sudoku'

solve :: Sudoku -> Sudoku
solve sudoku = fromJust $ guess sudoku

-- Debugging Functions

disp :: Sudoku -> IO ()
disp sudoku = mapM_ print list
    where
        list = f [] $ map (value . flip get sudoku) positions

        f xs [] = reverse xs
        f xs ns = f (take 9 ns : xs) (drop 9 ns)

disp2 :: Sudoku -> IO ()
disp2 sudoku = mapM_ print $ zip (cycle [1..9]) list
    where
        list = map (flip get sudoku) positions

listify :: (Show a) => [a] -> String
listify [] = ""
listify (x:xs) = show x ++ "\n" ++ listify xs

str :: Sudoku -> String
str sudoku = "\n" ++ listify list
    where
        list = f [] $ map (value . flip get sudoku) positions

        f xs [] = reverse xs
        f xs ns = f (take 9 ns : xs) (drop 9 ns)

str2 :: Sudoku -> String
str2 sudoku = (++) "\n" $ listify $ zip (cycle [1..9]) list
    where
        list = map (flip get sudoku) positions
