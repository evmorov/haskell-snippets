main = do
  let tree = (Node 2 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
  print $ treeDepth tree
  print $ treeSum tree
  print $ isSortedTree tree 1 5
  let newTree = addNewMax tree
  print $ newTree
  let newTree2 = insertOrder newTree 3
  print $ newTree2

data Tree
  = Leaf
  | Node Int
         Tree
         Tree
  deriving (Show)

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node num leftSubtree rightSubtree) = num + treeSum leftSubtree + treeSum rightSubtree

-- For each Node, we have to check the value in it is between the
-- min and max values, which start off as far apart as possible,
-- then get split into smaller ranges based on the value at the Node
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node num leftSubtree rightSubtree) minVal maxVal =
  let leftSorted = isSortedTree leftSubtree minVal num
      rightSorted = isSortedTree rightSubtree num maxVal
  in num >= minVal && num < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node num t1 Leaf) = Node num t1 (Node (num + 1) Leaf Leaf)
addNewMax (Node num t1 t2) = Node num t1 (addNewMax t2)

insertOrder :: Tree -> Int -> Tree
insertOrder Leaf new = Node new Leaf Leaf
insertOrder (Node num left right) new
  | new < num = Node num (insertOrder left new) right
  | otherwise = Node num left (insertOrder right new)
