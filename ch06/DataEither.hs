data Maybe a = Nothing
  | Just a
    deriving (Eq, Ord, Read, Show)

data Either a b = First a
                | Second b
                deriving (Eq, Ord, Read, Show)
