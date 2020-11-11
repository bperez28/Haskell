import Data.List
import Control.Monad
import System.IO
import Data.Char
import Data.Maybe
import System.Directory
import System.Environment
import Data.Tree

data Student = Student {
firstName :: String,
 lastName :: String,
 major :: String,
 age :: Int} deriving (Show)


createNode :: Student -> TreeNode
createNode x = TreeNode x EmptyTree EmptyTree

data TreeNode = EmptyTree | TreeNode Student TreeNode  TreeNode deriving(Show)

treeInsert :: Student -> TreeNode -> TreeNode
treeInsert x EmptyTree = createNode x
treeInsert (Student fn1 ln1 mj1 ag1) (TreeNode (Student fn2 ln2 mj2 ag2) left right)
  | ag1 <= ag2  = TreeNode (Student fn2 ln2 mj2 ag2) (treeInsert (Student fn1 ln1 mj1 ag1) left) right
  | ag1 > ag2  = TreeNode (Student fn2 ln2 mj2 ag2) left (treeInsert (Student fn1 ln1 mj1 ag1) right)


view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

searchForAge :: Student -> TreeNode -> Bool
searchForAge x EmptyTree = False
searchForAge (Student a1 b1 c1 age1) (TreeNode (Student a2 b2 c2 age2) left right)
  | age1 == age2 = True
  | age1 < age2  = searchForAge (Student a2 b2 c2 age1) left
  | age1 > age2  = searchForAge (Student a2 b2 c2 age1) right
