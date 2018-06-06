import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction [] _ = 0.0
            foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (x - y):ys
            foldingFunction (x:y:ys) "/" = (x / y):ys
            foldingFunction xs numberString = read numberString:xs
