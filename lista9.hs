--Marek Padlewski

--zadanie 1

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Eq,Ord,Show,Read)

isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node l r) =
  abs(numOfLeaves l - numOfLeaves r) <= 1 && isBalanced l && isBalanced r
  where numOfLeaves (Leaf _) = 1
        numOfLeaves (Node l r) = numOfLeaves l + numOfLeaves r

tree1 = Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 6) (Leaf 7))
tree2 = Node (Leaf 1) (Node (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4)) (Leaf 5))
tree3 = Leaf 0

t11 = isBalanced(tree1)
t12 = isBalanced(tree2)
t13 = isBalanced(tree3)


data BTree a = BLeaf | BNode (BTree a) a (BTree a)
             deriving (Eq,Ord,Show,Read)

tree4 = BNode (BNode BLeaf 2 BLeaf) 1 (BNode BLeaf 3 BLeaf)
tree5 = BNode BLeaf 1 (BNode (BNode (BNode BLeaf 2 BLeaf) 3 BLeaf) 4 BLeaf)
tree6 = BLeaf

--zadanie 3a

intPathLen :: BTree a -> Int
intPathLen a = ipl a 0
  where
    ipl :: BTree a -> Int -> Int
    ipl (BLeaf) _ = 0
    ipl (BNode l v r) i = i + ipl l (i+1) + ipl r (i+1)


t3a1 = intPathLen(tree4)
t3a2 = intPathLen(tree5)
t3a3 = intPathLen(tree6)


--zadanie 3b

extPathLen :: BTree a -> Int
extPathLen a = epl a 0
  where
    epl :: BTree a -> Int -> Int
    epl (BNode l v r) i = epl l (i+1) + epl r (i+1) 
    epl (BLeaf) i = i


t3b1 = extPathLen(tree4)
t3b2 = extPathLen(tree5)
t3b3 = extPathLen(tree6)

