import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- consider this tree
freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
       )
       (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
       )

-- if we wanted to change the W to a P, one way to do this is pattern matching but it is confusing:
changeToP1 :: Tree Char -> Tree Char
changeToP1 (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

-- another way is to use directions (either left or right) but this is inefficient since we always start from the root:
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP2 :: Directions -> Tree Char -> Tree Char
changeToP2 (L:ds) (Node x l r) = Node x (changeToP2 ds l) r
changeToP2 (R:ds) (Node x l r) = Node x l (changeToP2 ds r)
changeToP2 [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
-- the result of elemAt [R,L] $ changeToP2 [R,L] freeTree is 'P'

-- a better way is to use breadcrumbs:
type Breadcrumbs1 = [Direction]

goLeft1 :: (Tree a, Breadcrumbs1) -> (Tree a, Breadcrumbs1)
goLeft1 (Node _ l _, bs) = (l, L:bs)

goRight1 :: (Tree a, Breadcrumbs1) -> (Tree a, Breadcrumbs1)
goRight1 (Node _ _ r, bs) = (r, R:bs)

-- a helper function from chapter 13 that provides a more natural way of applying functions in this case
x -: f = f x

-- the result of (freeTree, []) -: goRight1 -: goLeft1 is (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

-- instead of Direction, let's use a crumb with a direction, the value from the node we came from and the subtree that we didn't visit
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- breadcrumbs now is a list of Crumb
type Breadcrumbs a = [Crumb a]

-- goLeft and goRight now need to take account of the paths we didn't take:
goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

-- going up plugs the previous focus into the 'hole'
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- a pair that contains a focused part of a data structure and its surroundings is called a zipper
type Zipper a = (Tree a, Breadcrumbs a)

-- modify modifies the element int the root of the subtree under focus:
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- we can do stuff like this: (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')

-- attach replaces a subtree, so we can add a Node to an empty leaf:
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- we can get to the top of the tree recursively:
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

-- how would a zipper look for lists? the structure is simpler than for trees
-- the first list represents the list that we’re focusing on, and the second list is the list of breadcrumbs
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)


-- let's demonstrate how zippers can work with a simple filesystem:
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

-- some example files and folders:
myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

-- crumb contains the parent folder name and the items that come before and after the current focus, leaving a 'hole' between
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- given a name, focus on a file or folder that’s located in the current focused folder
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- we can walk to a file or folder like this:
newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- move up and focus on a neighbouring file:
newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"

-- rename the currently focused file or folder:
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

newFocus3 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

-- make a new item in the current folder:
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

newFocus4 = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp


-- the tree operations could do with error handling - let's use the Maybe monad:
goLeftM :: Zipper a -> Maybe (Zipper a)
goLeftM (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeftM (Empty, _) = Nothing

goRightM :: Zipper a -> Maybe (Zipper a)
goRightM (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRightM (Empty, _) = Nothing

goUpM :: Zipper a -> Maybe (Zipper a)
goUpM (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUpM (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUpM (_, []) = Nothing

-- like in ch13, we trade :- for >>= now that we are dealing with functions that return monads:
coolTree = Node 1 Empty (Node 3 Empty Empty)
-- returns Just (Node 3 Empty Empty,[RightCrumb 1 Empty])
expr1 = return (coolTree, []) >>= goRightM
-- returns Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])
expr2 = return (coolTree, []) >>= goRightM >>= goRightM
-- returns Nothing
expr3 = return (coolTree, []) >>= goRightM >>= goRightM >>= goRightM