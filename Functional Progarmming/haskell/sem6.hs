main :: IO ()
main = do
    print (maxDepthBlueNode colorTree)

data BTree a
  = Empty
  | Node a (BTree a) (BTree a)
  deriving (Ord, Eq, Show)

t1 :: BTree Int
t1 =
  Node
    1
    (Node 2 (Node 5 Empty Empty) Empty)
    (Node 3 (Node 7 Empty Empty) (Node 6 Empty Empty))

sumTree :: Num a => BTree a -> a
sumTree Empty          = 0
sumTree (Node a lt rt) = a + sumTree lt + sumTree rt

getLevelsTree :: BTree a -> BTree (a, Int)
getLevelsTree = helper 0
  where
    helper _ Empty = Empty
    helper lev (Node v lt rt) =
      Node (v, lev) (helper (lev + 1) lt) (helper (lev + 1) rt)


mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Empty          = Empty
mapTree f (Node a lt rt) = Node (f a) (mapTree f lt) (mapTree f rt) 


data Color = Red | Green | Blue deriving (Read, Show, Eq)

colorTree :: BTree Color                                            --            Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)     --           /    \
                      (Node Red (Node Blue (Node Green Empty Empty) --        Red      Red
                                           (Node Red Empty Empty))  --        /        /  
                                Empty)                              --     Green     Blue  
                                                                    --               /   \
                                                                    --            Green  Red


inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node v lt rt) = inorder lt ++ [v] ++ inorder rt

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode bt =
  maximum [l | (c, l) <- inorder (getLevelsTree bt), c == Blue]