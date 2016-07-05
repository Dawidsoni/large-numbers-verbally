
{-
	Author: Dawid Wegner
	Language version: polish
-} 

module Slownie (Gender(..), Currency(..), getCurrency, slownie) where
	import Data.Map as Map
	import Data.List as List

	data Gender = Male | Female | Neuter deriving (Eq, Show)
	
	data Currency = Currency {
		baseForm     :: String,
		pluralForm   :: String,
		genitiveForm :: String,
		gender       :: Gender	
	} deriving Show
	
	data NumSuffix = NumSuffix {
		oneForm     :: String,
		twoToFourForm   :: String,
		othersForm :: String	
	} deriving Show	

	currencyMap :: Map String Currency	
	currencyMap = fromList [
		("AUD", Currency "dolar australijski" "dolary australijskie" "dolarów australijskich" Male),
		("BGN", Currency "lew" "lewy" "lewów" Male),
		("BRL", Currency "real brazylijski" "reale brazylijskie" "realów brazylijskich" Male),
		("BYR", Currency "rubel białoruski" "ruble białoruskie" "rubli białoruskich" Male),
		("CAD", Currency "dolar kanadyjski" "dolary kanadyjskie" "dolarów kanadyjskich" Male),
		("CHF", Currency "frank szwajcarski" "franki szwajcarskie" "franków szwajcarskich" Male),
		("CNY", Currency "yuan" "yuany" "yuanów" Male),
		("CZK", Currency "korona czeska" "korony czeskie" "koron czeskich" Female),
		("DKK", Currency "korona duńska" "korony duńskie" "koron duńskich" Female),
		("EUR", Currency "euro" "euro" "euro" Neuter),
		("GBP", Currency "funt szterling" "funty szterlingi" "funtów szterlingów" Male),
		("HKD", Currency "dolar hongkoński" "dolary hongkońskie" "dolarów hongkońskich" Male),
		("HRK", Currency "kuna" "kuny" "kun" Female),
		("HUF", Currency "forint węgierski" "forinty węgierskie" "forintów węgierskich" Male),
		("IDR", Currency "rupia indonezyjska" "rupie indonezyjskie" "rupii indonezyjskich" Female),
		("ISK", Currency "korona islandzka" "korony islandzkie" "koron islandzkich" Female),
		("JPY", Currency "jen" "jeny" "jenów" Male),
		("KRW", Currency "won południowokoreański" "wony południowokoreańskie" "wonów południowokoreańskich" Male),
		("MXN", Currency "peso meksykańskie" "peso meksykańskiego" "peso meksykańskich" Neuter),
		("MYR", Currency "ringgit" "ringgity" "ringgitów" Male),
		("NOK", Currency "korona norweska" "korony norweskie" "koron norweskich" Female),
		("NZD", Currency "dolar nowozelandzki" "dolary nowozelandzkie" "dolarów nowozelandzkich" Male),
		("PHP", Currency "peso filipińskie" "peso filipińskiego" "peso filipińskich" Neuter),
		("RON", Currency "lej" "leje" "lejów" Male),
		("RUB", Currency "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Male),
		("SDR", Currency "specjalne prawo ciągnięcia" "specjalne prawa ciągnięcia" "specjalnych praw ciągnięcia" Neuter),
		("PLN", Currency "złoty" "złote" "złotych" Male),
		("SEK", Currency "korona szwedzka" "korony szwedzkie" "koron szwedzkich" Female),
		("SGD", Currency "dolar singapurski" "dolary singapurskie" "dolarów singapurskich" Male),
		("THB", Currency "baht" "bahty" "bahtów" Male),
		("TRY", Currency "nowa lira turecka" "nowe liry tureckie" "nowych lir tureckich" Female),
		("UAH", Currency "hrywna" "hrywny" "hrywien" Female),
		("USD", Currency "dolar amerykański" "dolary amerykańskie" "dolarów amerykańskich" Male),
		("ZAR", Currency "rand" "randy" "randów" Male)]	
	
	numbersMap :: Map Integer String
	numbersMap = fromList [
		(0, "zero"),
		(1, "jeden"),
		(2, "dwa"),
		(3, "trzy"),
		(4, "cztery"),
		(5, "pięć"),
		(6, "sześć"),
		(7, "siedem"),
		(8, "osiem"),
		(9, "dziewięć"),
		(10, "dziesięć"),
		(11, "jedenaście"),
		(12, "dwanaście"),
		(13, "trzynaście"),
		(14, "czternaście"),
		(15, "piętnaście"), 
		(16, "szesnaście"),
		(17, "siedemnaście"),
		(18, "osiemnaście"),
		(19, "dziewiętnaście"),
		(20, "dwadzieścia"),
		(30, "trzydzieści"),
		(40, "czterdzieści"),
		(50, "pięćdziesiąt"),
		(60, "sześćdziesiąt"),
		(70, "siedemdziesiąt"),
		(80, "osiemdziesiąt"),
		(90, "dziewięćdziesiąt"),
		(100, "sto"),
		(200, "dwieście"),
		(300, "trzysta"),
		(400, "czterysta"),
		(500, "pięćset"),
		(600, "sześćset"),
		(700, "siedemset"),
		(800, "osiemset"),
		(900, "dziewięćset")]
		
	neuterOne :: String		
	neuterOne = "jedno"
		
	femaleOne :: String
	femaleOne = "jedna"
	
	femaleTwo :: String
	femaleTwo = "dwie"	
		
	minusSign :: String	
	minusSign = "minus"
	
	numBound :: Integer	
	numBound = 10 ^ 6000
	
	tooMuchString :: String	
	tooMuchString = "mnóstwo"
		
	thousSuffix :: NumSuffix	
	thousSuffix = NumSuffix "tysiąc" "tysiące" "tysięcy"
	
	lionSuffix :: NumSuffix 
	lionSuffix = NumSuffix "lion" "liony" "lionów"
	
	liardSuffix :: NumSuffix 	
	liardSuffix = NumSuffix "liard" "liardy" "liardów"
	
	basePrefixMap :: Map Integer String
	basePrefixMap = fromList [
		(1, "mi"),
		(2, "bi"),
		(3, "try"),
		(4, "kwadry"),
		(5, "kwinty"),
		(6, "seksty"),
		(7, "septy"),
		(8, "okty"),
		(9, "noni")]		
	
	extPrefixMap :: Map Integer String
	extPrefixMap = fromList [
		(1, "un"),
		(2, "do"),
		(3, "tri"),
		(4, "kwatuor"),
		(5, "kwin"),
		(6, "seks"),
		(7, "septen"),
		(8, "okto"),
		(9, "nowem"),
		(10, "decy"),
		(20, "wicy"),
		(30, "trycy"),
		(40, "kwadragi"),
		(50, "kwintagi"),
		(60, "seksginty"),
		(70, "septagi"),
		(80, "oktagi"),
		(90, "nonagi"),
		(100, "centy"),
		(200, "ducenty"),
		(300, "trycenty"),
		(400, "kwadryge"),
		(500, "kwinge"),
		(600, "sescenty"),
		(700, "septynge"),
		(800, "oktynge"),
		(900, "nonge")]		

	unfoldNum :: Integer -> [Integer]	
	unfoldNum num
		| num == 0			=	[]
		| hundredsNum > 0	=	hundredsNum : unfoldNum (num - hundredsNum)	
		| tensNum >= 20		=	tensNum : unfoldNum (num - tensNum)
		| otherwise			=	[num] where
			hundredsNum = num `div` 100 * 100
			tensNum = num `div` 10 * 10
	
	unfoldPrefix :: Integer -> [Integer]
	unfoldPrefix num = unfoldr aux (num, 1) where
		aux (n, k)
		 | n == 0		=	Nothing
		 | otherwise	=	Just(n `mod` 10 * k, (n `div` 10, k * 10))

	isPluralExc :: Integer -> Bool	
	isPluralExc num = modNum `elem` excs where
		modNum = abs num `mod` 100
		excs = [12, 13, 14]

	getSuffixForm :: Integer -> NumSuffix -> String
	getSuffixForm num suffix
		| num == 1						=	oneForm suffix 
		| isPluralExc num 				=	othersForm suffix
		| lstDgt >= 2 && lstDgt <= 4 	=	twoToFourForm suffix
		| otherwise						=	othersForm suffix where 
			lstDgt = num `mod` 10
			
	mapPrefix :: Integer -> String 
	mapPrefix num
	 | num == 0		=	""
	 | otherwise	=	extPrefixMap ! num
	
	unfoldNounPrefix :: Integer -> String 	
	unfoldNounPrefix pos
	 | pos < 10		=	basePrefixMap ! pos
	 | otherwise	=	concatMap mapPrefix $ unfoldPrefix pos
		
	unfoldNoun :: Integer -> Integer -> [String]
	unfoldNoun num pos 
	 | pos == 0		=	[]
	 | pos == 1		=	[getSuffixForm num thousSuffix]
	 | parity == 0	=	[prefix ++ getSuffixForm num lionSuffix]
	 | otherwise	=	[prefix ++ getSuffixForm num liardSuffix] where
		prefix = unfoldNounPrefix $ pos `div` 2
		parity = pos `mod` 2

	unfoldElem :: Integer -> Integer -> [String]	
	unfoldElem 0 _ = []	
	unfoldElem num pos = numString ++ nounString where 
		unfoldedNum = unfoldNum num
		numString = List.map (\x -> numbersMap ! x)  unfoldedNum
		nounString = unfoldNoun num pos
		
	absNumElem :: (Integer, Integer) -> Maybe([String], (Integer, Integer))
	absNumElem (0, _) = Nothing
	absNumElem (num, pos) = Just(numElem, (num', pos')) where
		numElem = unfoldElem (num `mod` 1000) pos
		num' = num `div` 1000
		pos' = pos + 1
	
	absNumToList :: Integer -> Integer -> [String]	
	absNumToList num pos = concat . reverse $ unfoldr absNumElem (num, pos)		
	
	numToList :: Integer -> [String]		
	numToList num
		| num == 0				=	[numbersMap ! 0]
		| abs num >= numBound	=	[tooMuchString]
		| num	 < 0			=	minusSign : absNumToList (-num) 0
		| otherwise				=	absNumToList num 0
	
	currForm :: Currency -> Integer -> String
	currForm curr num
	 | absNum == 1						=	baseForm curr
	 | isPluralExc num 					=	genitiveForm curr
	 | lstDgt >= 2 && lstDgt <= 4		=	pluralForm curr
	 | otherwise			  			= 	genitiveForm curr where
		absNum = abs num
		lstDgt = absNum `mod` 10
	
	replaceLast :: [String] -> String -> [String]	
	replaceLast list str = take (len - 1) list ++ [str] where
		len = length list
	
	matchListToCurr :: Currency -> Integer -> [String] -> [String]	
	matchListToCurr (Currency _ _ _ gender) num list
	 | gender == Neuter && isOneCase	=	updateList neuterOne
	 | gender == Female && isOneCase	=	updateList femaleOne
	 | gender == Female && isTwoCase	=	updateList femaleTwo 
	 | otherwise						=	list where
		updateList = replaceLast list
		absNum = abs num
		modNum = absNum `mod` 100
		isOneCase = absNum == 1
		isTwoCase = absNum `mod` 10 == 2 && modNum /= 12
	
	listWithCurr :: Currency -> Integer -> [String] -> [String]
	listWithCurr curr num list 
	 | abs num >= numBound	= list
	 | otherwise 			= currList ++ [currString] where
		currList = matchListToCurr curr num list
		currString = currForm curr num
	
	listToString :: [String] -> String	
	listToString list = concat $ aux list where
		aux [x] = [x]
		aux (x:xs) = (x : " " : aux xs)
				
	getCurrency :: String -> Currency 
	getCurrency curr = currencyMap ! curr
		
	slownie :: Currency -> Integer -> String	
	slownie curr num = listToString currList where
		numList = numToList num
		currList = listWithCurr curr num numList
