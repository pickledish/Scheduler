module Main where

import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Data.List

type TaskMap = Map.Map TaskID Task


---- SchedItem = (TaskID, Proc, StartTime)
type SchedItem = (Int, Int, Int)

type Task = (Int, Int, [Int])
type TaskID = Int

getID (id, _, _) = id
getTime (_, time, _) = time
getDeps (_, _, list) = list

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

readSched :: String -> SchedItem
readSched s = (id, proc, time)
    where [id, proc, time] = map readInt $ splitStr ' ' s

readTask :: String -> Task
readTask s = (id, length, tail deps)
    where id : length : deps = map readInt $ splitStr ' ' s

fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x

-------------------------------------------------------------------------------

-- Takes in a scheduling and a task
-- Returns the individual scheduling item where that task is to be executed

findTask :: [SchedItem] -> Task -> SchedItem
findTask schedule task = fromMaybe (head schedule) maybe
    where maybe = find (\sched -> fst3 sched == getID task) schedule


-- Takes in a scheduling, and two task tuples
-- Returns whichever task finished later, according to the schedule

later :: [SchedItem] -> Task -> Task -> Task
later schedule t1 t2 = 
    let [runTime1, runTime2] = map getTime [t1, t2]
        [sched1, sched2] = map (findTask schedule) [t1, t2]
        [start1, start2] = map trd3 [sched1, sched2]
        [total1, total2] = [runTime1 + start1, runTime2 + start2]
    in if total1 > total2 then t1 else t2


-- Takes in a task and a questionable parent
-- Returns whether task depended on that parent as a boolean

hasDep :: Task -> Task -> Bool
hasDep task parent = elem (getID task) (getDeps parent)


getLast :: [SchedItem] -> [Task] -> Maybe Task
getLast _ [] = Nothing
getLast s t = Just $ foldl (later s) (head t) t


-- Takes in a scheduling, a task map, and a task ID T
-- Returns, of all tasks that T depended on, the one that finished last

getLastDep :: [SchedItem] -> TaskMap -> Task -> Maybe Task
getLastDep schedule tasks task = 
    let relevTasks = filter (hasDep task) (Map.elems tasks)
    in getLast schedule relevTasks

-------------------------------------------------------------------------------

makeStory :: [SchedItem] -> TaskMap -> Maybe Task -> String -> String
makeStory _ _ Nothing currentString = currentString
makeStory schedule tasks (Just t) currentString =
    let nextTask = getLastDep schedule tasks t
        nextString = currentString ++ "\n" ++ (show t)
    in makeStory schedule tasks nextTask nextString

alg :: [SchedItem] -> [Task] -> String
alg schedule tasks = 
    let taskMap = Map.fromList $ map ( \task -> (getID task, task) ) tasks
        endOfPath = getLast schedule tasks
    in makeStory schedule taskMap endOfPath ""

main :: IO ()
main = do

    putStrLn "Thinking...2"

    [grFile, outFile] <- getArgs
    grContents <- readFile grFile
    schedule <- readFile outFile

    let taskItems = map readTask $ tail $ splitStr '\n' grContents
    let schedItems = map readSched $ splitStr '\n' schedule

    putStrLn $ show $ alg schedItems taskItems
    putStrLn "Done"













