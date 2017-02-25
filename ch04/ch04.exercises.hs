import System.Environment (getArgs)

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

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of 
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
          myFunction = firstWords
