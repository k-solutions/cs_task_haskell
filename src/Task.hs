module Task where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List                (foldl', union, unionBy)
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Text                (Text)

type Name = Text
type ShellCmd = Text

newtype Command m = Command
                  { runCommand :: Text -> m ()
                  }

data Task = Task
          { _tName     :: !Name
          , _tCommand  :: !ShellCmd
          , _tRequires :: [Name]
          } deriving (Show, Eq, Ord)

makeLenses ''Task

data PriorityTask = PriorityTask
                  { _pTask      :: Task
                  , _ptPriority :: Int
                  } deriving (Show, Eq)

makeLenses ''PriorityTask

instance Ord PriorityTask where
  t1 <= t2 = t1 ^. ptPriority <= t2 ^. ptPriority

--- Helper Types ---

orderedTasks :: (Contravariant f, Applicative f) => (Name -> f Name) -> Task -> f Task
orderedTasks = folding (\s -> s ^.. tRequires . folded
                           <> s ^.. tName
                       )

type TaskMap = Map Name Task
type IdxTaskMap = Map Name (Int, Task)
type IdxState = (Int, IdxTaskMap)
type MapState = (IdxState, [[Name]])

--- Multythreaded implementation ---

--- Lenses Implementaion ---

type LensState = State TaskMap (Int, [[Name]])

--- State Implementation ---

type StateTpl  =  ([[Name]], IdxTaskMap)
type AsState a = State IdxTaskMap [a]

sortWithTasks :: [Task] -> AsState Task
sortWithTasks = mapState fromNameToTask
              . toTaskMapWithState

toTaskMapWithState :: [Task] -> AsState [Name]
toTaskMapWithState = zipWithM asTaskState [0..]

asTaskState :: Int -> Task -> AsState Name
asTaskState idx task@Task {} = do
   taskMap <- get
   let ((_, newM), newTaskList) = stepFn task ((idx, taskMap), [])
   put newM
   return $ concat newTaskList

asNameState :: [Name] -> [Name] -> AsState Task
asNameState tl tr = do
    taskMap <- get
    return $ toTaskUnion taskMap tl tr

--- Full data implementation ---

sortByRequires :: [Task] -> Maybe [Task]
sortByRequires ts = toSortedTask                   -- 3. Find tasks from result list tasks names
                  $ toTaskNameLst                  -- 2. Merge the each individual task views into global task list
                  $ foldr stepFn (initAcc ts) ts   -- 1. Transform to (([[task name]], length), Map task name (priority, task))
                                                   -- where [[task name]] is list of how each task sees the task priority


--- Helpers ---

fromNameToTask :: ([[Name]], IdxTaskMap) -> ([Task], IdxTaskMap)
fromNameToTask (taskLists, taskMap) = (tasks taskLists, taskMap)
  where
    taskUnionStep :: [Name] -> ([Task], [Name]) -> ([Task], [Name])
    taskUnionStep it (acc, t') = (newAcc, it)
      where newAcc = toTaskUnion taskMap t' it `union` acc

    tasks :: [[Name]] -> [Task]
    tasks []        = []
    tasks [nameLst] = map (\n -> snd $ taskMap M.! n) nameLst
    tasks (t:ts)    = fst $ foldr taskUnionStep ([], t) ts

toTaskUnion :: IdxTaskMap -> [Name] -> [Name] -> [Task]
toTaskUnion _ [] []         = []
toTaskUnion taskMap tls trs = go (tls, trs) []
  where
    fromMap :: Name -> (Int, Task)
    fromMap = (taskMap M.!)

    toTask  = snd . fromMap

    go :: ([Name], [Name]) -> [Task] -> [Task]
    go ([], trs') acc = reverse acc <> map toTask trs'
    go (tls', []) acc = reverse acc <> map toTask tls'
    go (tl:tls', tr:trs') acc
      | tl1 == tr1 = go (tls', trs') (tl2 : acc)
      | tl1 > tr1  = go (tls', trs') $ newAcc tr2 tl2 acc
      | otherwise  = go (tls', trs') $ newAcc tl2 tr2 acc
      where
        newAcc it1 it2 acc
          | it1 `elem` acc && it2 `elem` acc = acc
          | it1 `elem` acc = it2:acc
          | it2 `elem` acc = it1:acc
          | otherwise = it2:it1:acc

        (tl1, tl2) = fromMap tl
        (tr1, tr2) = fromMap tr

initState :: StateTpl
initState = ([], M.empty)

formTaskMap :: [Task] -> IdxTaskMap
formTaskMap = M.fromList
            . zipWith toMapList [0..]
   where
    toMapList :: Int -> Task -> (Name, (Int, Task))
    toMapList n t@Task {..} = (_tName, (n, t))


getTask :: IdxTaskMap -> Name -> Maybe Task
getTask tm taskName = case tm M.!? taskName of
                        Just (_, t) -> Just t
                        Nothing     -> Nothing

toSortedTask :: (IdxTaskMap, [Name]) -> Maybe [Task]
toSortedTask (tm, tl) = mapM (getTask tm) tl

toTaskNameLst :: MapState -> (IdxTaskMap, [Name])
toTaskNameLst ((_, tm), taskNames) = (tm, foldr1 (stepFn' tm) taskNames)

stepFn' :: IdxTaskMap -> [Name] -> [Name] -> [Name]
stepFn' tm = unionBy unionStepFn
  where
      mapV :: Name -> Int
      mapV = fst . (tm M.!)

      unionStepFn :: Name -> Name -> Bool
      unionStepFn t1 t2 =
          case compare (mapV t1) (mapV t2) of
            EQ -> True
            _  -> False

initAcc :: [Task] -> MapState
initAcc ts = ((length ts, M.empty), [])                    -- We accumalate all tasks orders in a List of List and Map Name Int

stepFn :: Task -> MapState -> MapState
stepFn it@Task {..} ((idx, m), acc) = ((newIdx, newMap), newList)
  where
      newIdx  = idx + 1
      newMap  = M.insert _tName (idx, it) m
      newList = (_tRequires <> [_tName]) : acc

--- Test Helpers ---

testTask1 = Task "test1" "touch /tmp/file1" []

testTasks = [ testTask1
            , Task "test2"  "cat /tmp/file1"   [ "task3" ]
            , Task "task3"  "echo 'Hello World!' > /tmp/file1" [ "test1" ]
            , Task "task4"  "rm /tmp/file1"  ["test2", "task3"]
            ]
