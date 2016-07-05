import System.Environment
import Slownie

{- compile with: ghc -o slownie slownie.hs -}

getName :: Integer -> String -> String
getName num curr = slownie (getCurrency curr) num

getNameWithArgs :: [String] -> String
getNameWithArgs args = getName num curr where
	["slownie", strNum, curr] = args
	num = read strNum :: Integer

printName :: Integer -> String -> IO ()
printName num curr = putStrLn $ getName num curr

main :: IO ()
main = do
	args <- getArgs
	putStrLn $ getNameWithArgs args

{- some tests -}

test1 = printName (10 ^ 6000) "PLN"

test2 = printName (-(10 ^ 6000)) "PLN"

test3 = mapM_ putStrLn list where
	list = map (\n -> getName n "PLN") [(-25)..25]
	
test4 = mapM_ putStrLn list where
	list = map (\n -> getName n "CZK") [(-25)..25]
	
test5 = mapM_ putStrLn list where
	list = map (\n -> getName n "EUR") [(-25)..25]

test6 = mapM_ putStrLn list where
	list = map (\n -> getName n "PLN") [99..125]

test7 = mapM_ putStrLn list where
	list = map (\n -> getName n "CZK") [99..125]

test8 = mapM_ putStrLn list where
	list = map (\n -> getName n "EUR") [99..125]

test9 = mapM_ putStrLn list where
	list = map (\n -> getName n "PLN") [999..1025]

test10 = mapM_ putStrLn list where
	list = map (\n -> getName n "PLN") [2499..2525]

test11 = mapM_ putStrLn list where
	list = map (\n -> getName n "PLN") [2499..2525]
	
test12 = printName ((10 ^ 6) + (10 ^ 4) + 2) "CZK"

test13 = printName (2 *(10 ^ 9)) "EUR"

test14 = printName ((2 * (10 ^ 11)) + 13 * (10 ^ 9)) "CZK"

test15 = printName 123456789 "CZK"

test16 = printName 2000123456789 "CZK"

test17 = mapM_ putStrLn list where
	list = map (\n -> getName (10 ^ n) "PLN") examples
	examples = [12, 15, 18, 24, 48, 69, 99, 120, 150, 201, 216, 240, 282, 
				333, 369, 378, 399, 450, 510, 600, 666, 999, 1197, 5997]
	
test18 = printName ((12 * (10 ^ 5997)) + (3 * (10 ^ 1197)) + 1) "CZK"

test19 = printName ((12 * (10 ^ 5997)) + (3 * (10 ^ 1197)) + 2) "CZK"
	
test20 = printName (-((10 ^ 6000) - 1)) "PLN"



