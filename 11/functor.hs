import Data.Char
import Data.List

main2 = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

main1 = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
