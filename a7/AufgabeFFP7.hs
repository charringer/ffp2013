module AufgabeFFP7 where
import Data.Array

data Color = Black | White deriving (Show,Eq)
data Digit = One | Two | Three | Four | Five | Six | Seven | Eight |Nine | Blank deriving (Show,Eq)
type Str8ts = Array (Int,Int) (Color,Digit)
type ColorOut = Char
type DigitOut = Int
type Str8tsOut = Array (Int,Int) (ColorOut, DigitOut)

pair a b = (a,b)
start :: (Int,Int)
start = (1,1)
end :: (Int,Int)
end   = (9,9)
inds = range (start,end)

pList1 l = map fst l
pList2 l = map snd l

nonBlankDigits :: [Int]
nonBlankDigits = [1..9]

emptyStr8ts = array (start,end) (zipWith pair inds [(White,Blank) | _<-[0..]])
a = emptyStr8ts//[((1,1),(White,One)), ((1,2),(White,Two))]
b = convertField emptyStr8ts


convertColor c
	| c == White = 'w'
	| c == Black = 'b'
convertDigit d
	| d == One = 1
	| d == Two = 2
	| d == Three = 3
	| d == Four = 4
	| d == Five = 5
	| d == Six = 6
	| d == Seven = 7
	| d == Eight = 8
	| d == Nine = 9
	| d == Blank = 0
	
convertEntry (c,d) = (convertColor c, convertDigit d)
convertField field = array (start,end) [(x,convertEntry (field!x)) | x<-inds]

getRows field = [[field!(x,y) | (x,y)<-inds, x==n] | n<-[1..9]]
getCols field = [[field!(x,y) | (x,y)<-inds, y==n] | n<-[1..9]]

occursAtMostOnce d symbol = length (filter (==symbol) d) <= 1

isValid field = all isValidList (getRows field) && all isValidList (getCols field)

isValidList l = all (occursAtMostOnce (pList2 l)) nonBlankDigits && all isValidSubList (map pList2 (splitList l)) 

isBlack (c,d) = (c=='b')
isWhite (c,d) = (c=='w')

splitList c = splitList_ (dropWhile isBlack c)

splitList_ [] = []
splitList_ c = (takeWhile isWhite c) : splitList (dropWhile isBlack (dropWhile isWhite c))

isValidSubList l
	| nonBlankPart == [] = True
	| otherwise = (maximum nonBlankPart) - (minimum nonBlankPart) + 1 <= length l
	where nonBlankPart = filter (/=0) l

isBlank (c,d) = d == 0

makeSolutions field = foldl (++) [field] (map makeSolutions (expand field))

isComplete field = [(x,field!x) | x<-inds,isBlank (field!x)] == []
	
expand field 
	| isFilled = [field]
	| otherwise = filter isValid (map setMinIndTo nonBlankDigits)
	where
		blanks = [(x,field!x) | x<-inds,isBlank (field!x)]
		isFilled = blanks == []
		(minInd,(color,_)) = head blanks
		setMinIndTo d = field//[(minInd,(color,d))]
		
dispRows field = map pList2 (getRows field)

fastStr8ts field
	| solutions == [] = field_
	| otherwise = head solutions
	where
		field_ = convertField field
		solutions = filter isComplete $ makeSolutions field_
