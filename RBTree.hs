--technically, this is not a Red Black Tree as defined in CLRS, as the root is not forced to be black
--but that rule isn't actually necesary, and isn't really compatible with the basic architecture
--as each child is a full-fledged tree in and of itself

data Color = Red | Black deriving (Eq, Show)

--data Tree val = Leaf | Node (Tree val) (Tree val) val

--data KeyTree val key = Leaf | Node (

data RBTree key val = Leaf | Node (RBTree key val) (RBTree key val) key val Color deriving (Show)

testTree = (Node (Node Leaf Leaf 5 6 Red) (Node Leaf Leaf 12 12 Red) 8 (-3) Black)
testInvalidTree = (Node (Node (Node Leaf Leaf 2 12 Black) Leaf 3 100 Black) Leaf 12 12 Black)
testData = [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16),(-2,5),(453,-453),(435,5356),(-2321,54),(45,32),(345,-5555)]


getColor :: (RBTree key val) -> Color
getColor Leaf = Black
getColor (Node _ _ _ _ c) = c

getKey :: (RBTree key val) -> (Maybe key)
getKey Leaf = Nothing
getKey (Node _ _ k _ _) = Just k

getValue :: (RBTree key val) -> (Maybe val)
getValue Leaf = Nothing
getValue (Node _ _ _ v _) = Just v

isBlack :: (RBTree key val) -> Bool
isBlack t = getColor t == Black

isRed :: (RBTree key val) -> Bool
isRed t = getColor t == Red

otherColor :: Color -> Color
otherColor Red = Black
otherColor Black = Red

--does nothing to a leaf
--don't call on leaves
invertColor :: (RBTree key val) -> (RBTree key val)
invertColor Leaf = Leaf
invertColor (Node l r k v c) = (Node l r k v (otherColor c))

asBlack :: (RBTree key val) -> (RBTree key val)
asBlack Leaf = Leaf
asBlack (Node l r k v c) = (Node l r k v Black)

asRed :: (RBTree key val) -> (RBTree key val)
asRed Leaf = Leaf
asRed (Node l r k v c) = (Node l r k v Red)

--for the puproses of these functions, leaves are considered to have leaves for children
--this way I don't need to return a Maybe RBTree
--don't let this lead you to infinite recursion
leftChild :: (RBTree key val) -> (RBTree key val)
leftChild Leaf = Leaf
leftChild (Node l _ _ _ _) = l

rightChild :: (RBTree key val) -> (RBTree key val)
rightChild Leaf = Leaf
rightChild (Node _ r _ _ _) = r

height Leaf = 0
height (Node l r _ _ _) = max (height l) (height r) + 1

flattenKeys :: (RBTree key val) -> [key]
flattenKeys Leaf = []
flattenKeys (Node l r k _ _) = (flattenKeys l) ++ (k : (flattenKeys r))

flatten :: (RBTree key val) -> [(key, val)]
flatten Leaf = []
flatten (Node l r k v _) = (flatten l) ++ ((k, v) : (flatten r))

get :: Ord key => (RBTree key val) -> key -> (Maybe val)
get Leaf _ = Nothing
get (Node l r k v c) key1 
    | key1 == k = Just v
    | key1 < k  = get l key1
    | key1 > k  = get r key1

containsKey :: Ord key => (RBTree key val) -> key -> Bool
containsKey Leaf _ = False
containsKey (Node l r k v c) key1
    | key1 == k = True
    | key1 < k  = containsKey l key1
    | key1 > k  = containsKey r key1

containsValue :: (Eq val) => (RBTree key val) -> val -> Bool
containsValue Leaf _ = False
containsValue (Node l r k v c) val1 =
    if v == val1
        then True
        else or [(containsValue l val1), (containsValue r val1)]

blackHeight :: (RBTree key val) -> (Maybe Int)
blackHeight Leaf = Just 1
blackHeight (Node l r _ _ c) =
        case leftHeight of
            Nothing -> Nothing
            Just lh -> case rightHeight of
                Nothing -> Nothing
                Just rh -> if lh == rh
                    then Just (lh + selfIsBlack)
                    else Nothing
    where leftHeight = blackHeight l
          rightHeight = blackHeight r
          selfIsBlack 
            | c == Black = 1
            | otherwise  = 0

validateRBTree :: (RBTree key val) -> Bool
validateRBTree t =
        case h of
            Nothing -> False
            Just _ -> True
    where h = blackHeight t

--invaled rotations return Leaf; valid rotations never will
--this is easier to work with than Maybe
rotl :: (RBTree key val) -> (RBTree key val)
rotl Leaf = Leaf
rotl (Node _ Leaf _ _ _) = Leaf
rotl (Node a (Node b c ky vy cy) kx vx cx) =
    (Node (Node a b kx vx cx) c ky vy cy)

rotlAndColor :: (RBTree key val) -> (RBTree key val)
rotlAndColor Leaf = Leaf
rotlAndColor (Node _ Leaf _ _ _) = Leaf
rotlAndColor (Node a (Node b c ky vy cy) kx vx cx) =
    (Node (Node a b kx vx (otherColor cx)) c ky vy (otherColor cy))

rotr :: (RBTree key val) -> (RBTree key val)
rotr Leaf = Leaf
rotr (Node Leaf _ _ _ _) = Leaf
rotr (Node (Node a b kx vx cx) c ky vy cy) =
    (Node a (Node b c ky vy cy) kx vx cx)

rotrAndColor :: (RBTree key val) -> (RBTree key val)
rotrAndColor Leaf = Leaf
rotrAndColor (Node Leaf _ _ _ _) = Leaf
rotrAndColor (Node (Node a b kx vx cx) c ky vy cy) =
    (Node a (Node b c ky vy (otherColor cy)) kx vx (otherColor cx))

assign :: (Ord key) => (RBTree key val) -> (key, val) -> (RBTree key val)
assign Leaf (k, v) = (Node Leaf Leaf k v Black)
assign (Node l r k1 v1 c) (k, v) = asBlack (hassign (Node l r k1 v1 c) (k, v))

hassign :: (Ord key) => (RBTree key val) -> (key, val) -> (RBTree key val)
hassign Leaf (k, v) = (Node Leaf Leaf k v Red)
hassign (Node l r k1 v1 c1) (k, v)
    | k == k1   = (Node l r k v c1)
    | k < k1    = fixTree (Node (hassign l (k, v)) r k1 v1 c1)
    | otherwise = fixTree (Node l (hassign r (k, v)) k1 v1 c1)

fixTree :: (RBTree key val) -> (RBTree key val)
fixTree Leaf = Leaf
fixTree (Node l r k1 v1 c1)
    | (isRed l) && (isRed (leftChild l)) =
        if isRed r
            then (Node (invertColor l) (invertColor r) k1 v1 Red)
            else rotrAndColor (Node l r k1 v1 c1)
    | (isRed l) && (isRed (rightChild l)) =
        if isRed r
            then (Node (invertColor l) (invertColor r) k1 v1 Red)
            else (Node (rotl l) r k1 v1 c1)
    | (isRed r) && (isRed (rightChild r)) =
        if isRed l
            then (Node (invertColor l) (invertColor r) k1 v1 Red)
            else rotlAndColor (Node l r k1 v1 c1)
    | (isRed r) && (isRed (leftChild r)) =
        if isRed l
            then (Node (invertColor l) (invertColor r) k1 v1 Red)
            else (Node l (rotr r) k1 v1 c1)
    | otherwise = (Node l r k1 v1 c1)



