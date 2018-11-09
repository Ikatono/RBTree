data Color = Red | Black deriving (Eq, Show)

--data Tree val = Leaf | Node (Tree val) (Tree val) val

--data KeyTree val key = Leaf | Node (

data RBTree key val = Leaf | Node (RBTree key val) (RBTree key val) key val Color deriving (Show)

--getColor :: RBTree val key -> Color
getColor Leaf = Black
getColor (Node _ _ _ _ c) = c

isBlack :: (RBTree key val) -> Bool
isBlack t = getColor t == Black

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
