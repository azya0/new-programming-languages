module Main where

data BTree a = Node { keys :: [a]
                     , children :: [BTree a]
                     , order :: Int 
                     } | Empty deriving (Show)

createTree :: Int -> BTree a
createTree _ = Empty

isEmpty :: BTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

splitChild :: Ord a => BTree a -> Int -> BTree a
splitChild parent idx = 
    let fullChild = children parent !! idx
        t = order parent
        midKey = keys fullChild !! (t - 1)
        leftKeys = take (t - 1) (keys fullChild)
        rightKeys = drop t (keys fullChild)
        leftChildren = take t (children fullChild)
        rightChildren = drop t (children fullChild)
        leftChild = Node leftKeys leftChildren t
        rightChild = Node rightKeys rightChildren t
        newKeys = (take idx (keys parent)) ++ [midKey] ++ (drop idx (keys parent))
        newChildren = (take idx (children parent)) ++ [leftChild, rightChild] ++ (drop (idx + 1) (children parent))
    in Node newKeys newChildren t

insertNonFull :: Ord a => BTree a -> a -> BTree a
insertNonFull Empty key = Node [key] [] 2
insertNonFull node key
    | null (children node) = 
        let newKeys = insertSorted (keys node) key
            maxKeys = 2 * (order node) - 1
        in if length newKeys > maxKeys
           then error "Node should be non-full"
           else Node newKeys [] (order node)
    | otherwise =
        let i = findInsertIndex (keys node) key
            child = children node !! i
            newChild = if length (keys child) == 2 * (order node) - 1
                       then let splitParent = splitChild node i
                                newI = if key > (keys splitParent !! i) then i + 1 else i
                            in insertNonFull (children splitParent !! newI) key
                       else insertNonFull child key
            newChildren = (take i (children node)) ++ [newChild] ++ (drop (i + 1) (children node))
        in Node (keys node) newChildren (order node)

insertSorted :: Ord a => [a] -> a -> [a]
insertSorted [] x = [x]
insertSorted (y:ys) x
    | x < y = x : y : ys
    | otherwise = y : insertSorted ys x

findInsertIndex :: Ord a => [a] -> a -> Int
findInsertIndex [] _ = 0
findInsertIndex (k:ks) key
    | key < k = 0
    | otherwise = 1 + findInsertIndex ks key

insert :: Ord a => BTree a -> a -> BTree a
insert Empty key = Node [key] [] 2
insert root key
    | length (keys root) == 2 * (order root) - 1 =
        let newRoot = Node [] [root] (order root)
            splitRoot = splitChild newRoot 0
            newChild = if key > head (keys splitRoot) 
                       then children splitRoot !! 1 
                       else children splitRoot !! 0
            updatedChild = insertNonFull newChild key
            finalChildren = if key > head (keys splitRoot)
                           then [head (children splitRoot), updatedChild]
                           else [updatedChild, last (children splitRoot)]
        in Node (keys splitRoot) finalChildren (order root)
    | otherwise = insertNonFull root key

inOrderTraversal :: BTree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal node = 
    let childVals = concatMap inOrderTraversal (children node)
        keyVals = keys node
    in interleave childVals keyVals
    where
        interleave [] keys = keys
        interleave (c:cs) keys = c : interleave cs keys

buildTree :: Ord a => Int -> [a] -> BTree a
buildTree t keys = foldl insert (createTree t) keys

testBTree :: IO ()
testBTree = do
    let values = [10, 20, 5, 6, 12, 30, 7, 17, 3, 8, 25]
    let tree = buildTree 2 values
    putStrLn "Построенное B-дерево (структура):"
    print tree
    putStrLn "\nОтсортированные ключи (in-order traversal):"
    print $ inOrderTraversal tree
    putStrLn "\nДобавляем ключи 15 и 1..."
    let tree2 = insert tree 15
    let tree3 = insert tree2 1
    putStrLn "Новые отсортированные ключи:"
    print $ inOrderTraversal tree3

main :: IO ()
main = testBTree