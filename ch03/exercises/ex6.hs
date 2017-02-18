import Data.List

listCompare first second = compare (length first) (length second)

listSort lst = sortBy listCompare lst
