-- Inf2d Assignment 1 2019-2020, Harry Lennox


module Connect4 where

import Data.List (sortBy, elemIndices, elemIndex)
import ConnectFourWithTwist


-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph= [Node]


-- | Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game   | checkWin game 1 = 1
            | checkWin game 0 = -1 
            | otherwise = 0

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 

-- The alphabeta function simply calls auxilary function maxValue, which performs all the heavy lifting.

alphabeta:: Role -> Game -> Int
alphabeta player game = maxValue game (-2) 2

{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}

-- removeExplored removes all branches where the head node of the branch is contained in the exploredList.

removeExplored::[Node] -> [Branch] -> [Branch]
removeExplored list [] = [] -- Empty case - return an empty list
removeExplored list (b:branches)    | notElem (head b) list = [b] ++ removeExplored list branches -- If the node to expand has not been explored, keep the branch and recurse
                                    | otherwise = removeExplored list branches                    -- Else discard it and continue

-- findSolution checks a list of branches to see if any of them reach the destination node.

findSolution::Node -> [Branch] -> Maybe Branch
findSolution n [] = Nothing -- If we run of branches to check, none of them have a solution so return nothing
findSolution n (b:bs)   | elemIndex n b == Nothing = findSolution n bs -- If the destination node is contained is not contained in the branch, this is not a solution so recurse with the next branch
                        | otherwise = Just b -- Otherwise this is a solution, so return it

-- These four auxilary functions implement the alpha beta pruning function. 

-- maxValue gives the maximised outcome for a given game state, with alpha beta pruning 

maxValue::Game -> Int -> Int -> Int
maxValue game alpha beta    | terminal game = eval game -- If the game is in a terminal state, its evaluation is our answer
                            | otherwise = maxLoop game alpha beta (-2) (movesAndTurns game 1) -- Otherwise, call maxLoop

-- maxLoop performs the recursive 'minimax' loop part of the algorithm, specifically it picks the best value for player max, given that player min picks the best value

maxLoop::Game -> Int -> Int -> Int -> [Game] -> Int
maxLoop game alpha beta v [] = v -- Once we have gone through all possible outcomes, pick the maximised one
maxLoop game alpha beta v (s:states)    | v' >= beta = v' -- If we find a better v' than beta, return it. This will never happen for beta = 2 and will only trigger on subsequent calls
                                        | otherwise = maxLoop game (max alpha v') beta v' states -- Otherwise recurse with the largest alpha we have
                                        where
                                            v' = max v (minValue s alpha beta) -- calls minValue so that maxValue can take its choice into account  

-- minValue gives the minimised outcome for a given game state, with alpha beta pruning

minValue::Game -> Int -> Int -> Int
minValue game alpha beta    | terminal game = eval game  -- If the game is in a terminal state, its evaluation is our answer
                            | otherwise = minLoop game alpha beta 2 (movesAndTurns game 0) -- otherwise, call minLoop

-- minLoop performs the recursive 'minimax' loop part of the algorithm, specifically it picks the best value for player min, given that player max picks the best value

minLoop::Game -> Int -> Int -> Int -> [Game] -> Int
minLoop game alpha beta v [] = v -- Once we have gone through all possible outcomes, pick the minimised one
minLoop game alpha beta v (s:states)    | v' <= alpha = v' -- If we find a better v' than alpha, return it. This will never happen for alpha = -2 and will only trigger on subsequent calls
                                        | otherwise = minLoop game alpha (min beta v') v' states -- Otherwise recurse with the smallest beta we have
                                        where
                                            v' = min v (maxValue s alpha beta) -- calls maxValue so that minValue can take its choice into account