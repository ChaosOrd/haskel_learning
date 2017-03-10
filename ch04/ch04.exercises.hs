import System.Environment (getArgs)
import Data.Char

interactWith function inputFile outputFile = do
    input <- readFile inputFile 
    writeFile outputFile (function input)

    
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

safeTail :: [a] -> Maybe [a]
safeTail a = if null a then Nothing
             else Just (tail a)


safeLast :: [a] -> Maybe a
safeLast a = if null a then Nothing
             else Just (last a)

safeInit :: [a] -> Maybe [a]
safeInit a = if null a then Nothing
             else Just (init a)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []       = [[]]
splitWith function a = if not (null (tail rest)) then [word] ++ (splitWith function (tail rest))
                       else [word]
                       where (word, rest) = break function a

firstWords a = unlines (map (head . words) (lines a))

asInt_fold :: String -> Int
asInt_fold ('-':s) = (-1) * asInt_fold s
asInt_fold s       = foldl step 0 s
    where step num ch = let result = num * 10 + digitToInt ch
                        in if result >= num then result else error "Integer overflow"

myConcat :: [[a]] -> [a]
myConcat lsts = foldr step [] lsts
    where step lst acc = lst ++ acc

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy eq lst = foldr step [] lst
    where step x (acc:accs) | eq (head acc) x = (x:acc):accs
                            | otherwise       = [x]:acc:accs
          step x []                           = [[x]]

myTakeWhileRecursion :: (a -> Bool) -> [a] -> [a]
myTakeWhileRecursion func (x:xs) | func x    = x:(myTakeWhileRecursion func xs)
                                 | otherwise = []
myTakeWhileRecursion func []                 = []

myTakeWhileFold :: (a -> Bool) -> [a] -> [a]
myTakeWhileFold func lst = foldr step [] lst
    where step x acc | func x    = x:acc
                     | otherwise = []


main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of 
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
          myFunction = firstWords
