import Data.Maybe

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

isEmpty Empty      = True
isEmpty a          = False

height (Node _ left right)    | isEmpty left && isEmpty right     = 1
height (Node _ left right)    | isEmpty left                      = 1 + height right
height (Node _ left right)    | isEmpty right                     = 1 + height left
height (Node _ left right)                                        = 1 + max (height left) (height right)
