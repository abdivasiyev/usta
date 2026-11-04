-- | Lanes for a commit graph, newest commit first: each commit sits in a
-- column, and each row says which lines cross it, so the rows can be drawn
-- one under another. A lane keeps its column while it lasts; a freed
-- column is taken by the next branch that needs one.
module Usta.Graph
  ( Row (..)
  , rows
  ) where

import Data.Kind (Type)
import Data.List (elemIndex, findIndex)
import Data.Maybe (isJust)

import Usta.Types.Insights (Node (..))

type Row :: Type
data Row = Row
  { column :: Int
    -- ^ Where the commit's dot is.
  , above :: [(Int, Int)]
    -- ^ Lines from the top of the row, at the first column, to the middle,
    -- at the second: lanes passing through, and those ending at the dot.
  , below :: [(Int, Int)]
    -- ^ Lines from the middle, at the first column, to the bottom, at the
    -- second: lanes passing through, and the dot's to its parents.
  , width :: Int
    -- ^ How many columns the row uses.
  , isMerge :: Bool
  }
  deriving stock (Eq, Show)

rows :: [Node] -> [Row]
rows = go []
  where
    go _ [] = []
    go lanes (n : rest) =
      let col = maybe (firstFree lanes) id (elemIndex (Just n.sha) lanes)
          -- Every lane waiting for this commit ends at its dot.
          ending = [i | (i, Just s) <- zip [0 ..] lanes, s == n.sha]
          passing = [i | (i, Just s) <- zip [0 ..] lanes, s /= n.sha]
          cleared = [if i `elem` ending || i == col then Nothing else l | (i, l) <- zip [0 ..] (pad col lanes)]
          (after, targets) = place cleared col n.parents
          above' = [(i, i) | i <- passing] <> [(i, col) | i <- ending]
          below' = [(i, i) | i <- passing] <> [(col, t) | t <- targets]
          used = maximum (0 : col : map fst above' <> map snd below') + 1
      in Row {column = col, above = above', below = below', width = used, isMerge = length n.parents > 1}
           : go (trim after) rest
    -- The first parent takes the dot's column; the others join a lane
    -- already waiting for them, or take a free column.
    place lanes _ [] = (lanes, [])
    place lanes col (p : ps) =
      let lanes1 = case elemIndex (Just p) lanes of
            Just _ -> lanes
            Nothing -> set col (Just p) lanes
          first = maybe col id (elemIndex (Just p) lanes1)
          (lanesN, others) = foldl join (lanes1, []) ps
      in (lanesN, first : reverse others)
    join (lanes, acc) p = case elemIndex (Just p) lanes of
      Just i -> (lanes, i : acc)
      Nothing -> let i = firstFree lanes in (set i (Just p) (pad i lanes), i : acc)
    firstFree lanes = maybe (length lanes) id (findIndex (not . isJust) lanes)
    pad i lanes = lanes <> replicate (i + 1 - length lanes) Nothing
    set i v lanes = [if j == i then v else l | (j, l) <- zip [0 ..] (pad i lanes)]
    trim = reverse . dropWhile (not . isJust) . reverse

