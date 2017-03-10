import Data.List
isInAny3 needle haystack = any (isInfixOf needle) haystack
isInAny4 needle haystack = any (needle `isInfixOf`) haystack
