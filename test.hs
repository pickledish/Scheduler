module Main where

import Data.List
import qualified Data.Set as Set
import System.Environment

-------------------------------------------------------------------------------
-- Useful Functions --
-------------------------------------------------------------------------------

readInt :: String -> Int
readInt x = read x :: Int

splitStr :: Char -> String -> [String]
splitStr _ "" = []
splitStr c s = 
    let first = takeWhile ( \x -> x /= c ) s
        lattr = dropWhile ( \x -> x /= c ) s
        safer = if (lattr == []) then [] else tail lattr 
    in  first : splitStr c safer


-------------------------------------------------------------------------------
-- The Types + Type Utilities --
-------------------------------------------------------------------------------

type Task = (Int, Int, [Int])
type TaskID = Int

getID (id, _, _) = id
getTime (_, time, _) = time
getDeps (_, _, list) = list

-- Represent (nProc, (proc, task), queued, waiting)
type State = (Int, [(Int, Task)], [Task], [Task])

decTime (procNum, (id, time, list)) = (procNum, (id, time - 1, list))
idOfPair pair = getID $ snd pair

showState :: State -> String
showState (_, running, queued, waiting) = 
    show running ++ "\n" ++ show queued ++ "\n" ++ show waiting ++ "\n\n"


-------------------------------------------------------------------------------
-- The Program --
-------------------------------------------------------------------------------


-- Takes mid-tick state with updated running but non-updated queued and waiting
-- Returns a pair, with (possibly) some waiting tasks moved to the queue

moveWaiting :: State -> ([Task], [Task])
moveWaiting (_, running, queued, waiting) = 
    let notYet = cantBegin $ (map snd running) ++ queued ++ waiting
        toQueue = filter (\id -> Set.notMember id notYet) (map getID waiting)
        (toMove, notMoved) = partition ( \t -> elem (getID t) toQueue ) waiting
    in  (queued ++ toMove, notMoved)


-- Takes in a valid state
-- Returns the state where "running" is exactly 1 time tick after the input

tick :: State -> State
tick (nProc, running, queued, waiting) = 
    let tickedDown = map decTime running
        running' = filter ( \pair -> (getTime $ snd pair) > 0 ) tickedDown
        (queued', waiting') = moveWaiting (nProc, running', queued, waiting)
    in (nProc, running', queued', waiting')


-- Takes in nProc and a freshly "ticked" state (see above)
-- Returns a new VALID state, with (possibly) some new processes running

moveQueued :: State -> State
moveQueued (nProc, running, queued, waiting) = 
    let toRun = zip (getProcLabels nProc running) queued
        running' = running ++ toRun
        queue' = drop (length toRun) queued
    in  (nProc, running', queue', waiting)


-- Takes in nProc and the list of currently running tasks
-- Returns a list of available labels for procs, which queued tasks can take

getProcLabels :: Int -> [(Int, Task)] -> [Int]
getProcLabels (-1) running = [mProcNum..]
    where mProcNum = if null running then 0 else maximum (map fst running) + 1
getProcLabels nProc running = [0..nProc] \\ (map fst running)


-- Takes in a list of not-done-yet tasks
-- Returns a Set (fast) of all tasks with unfinished dependencies (no run yet)

cantBegin :: [Task] -> Set.Set TaskID
cantBegin tasks = Set.fromList $ concat $ map getDeps tasks


-- Takes in a valid state
-- Returns a valid state, one time tick later, everything shifted as needed

step :: State -> State
step state = moveQueued $ tick state


-------------------------------------------------------------------------------
-- The IO Stuff --
-------------------------------------------------------------------------------

runProgram :: Int -> String -> State -> String
runProgram time soFar (_, [], [], []) = soFar
runProgram time soFar (nProc, running, queued, waiting) = 
    let (n', r', q', w') = step (nProc, running, queued, waiting)
        runningBefore = map idOfPair running
        newlyRunning = filter ( \p -> notElem (idOfPair p) runningBefore ) r'
        outStrings = concat $ map (format time) newlyRunning
    in runProgram (time + 1) (soFar ++ outStrings) (n', r', q', w')


format :: Int -> (Int, Task) -> String
format time newTask = taskID ++ " " ++ procNum ++ " " ++ (show time) ++ "\n"
    where (taskID, procNum) = (show $ getID $ snd newTask, show $ fst newTask)


parseTask :: String -> Task
parseTask input = (id, time, depList)
    where id : time : _ : depList = map readInt $ splitStr ' ' input


main :: IO ()
main = do

    putStrLn "Thinking..."

    [inFile, outFile, nProc] <- getArgs
    contents <- readFile inFile
    let lines = splitStr '\n' contents

    let [nodes, edges] = map readInt $ splitStr ' ' (head lines)
    let tasks = map parseTask $ tail lines
    let n = (readInt nProc) - 1
    let state = (n, [], [], tasks)

    writeFile outFile (runProgram 0 "" state)

    putStrLn "Done"


