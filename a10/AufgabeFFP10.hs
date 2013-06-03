import Data.Char
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)

type Buffer = (Int, String)

isValidBuffer :: Buffer -> Bool
isValidBuffer (pos, text) = pos <= length text

empty :: Buffer
empty = (0,[])

insert :: Char -> Buffer -> Buffer
insert c (pos, text) = (pos+1, (take pos text) ++ [c] ++ (drop pos text))

delete :: Buffer -> Buffer
delete (0, text) = (0, text)
delete (pos, text) = (pos - 1, (take (pos - 1) text) ++ (drop pos text))

atLeft :: Buffer -> Bool
atLeft (pos, _) = pos == 0

atRight :: Buffer -> Bool
atRight (pos, text) = pos == length text

left :: Buffer -> Buffer
left (pos, text)
	| atLeft (pos, text) = (pos, text)
	| otherwise = (pos - 1, text)
	
right :: Buffer -> Buffer
right (pos, text)
	| atRight (pos, text) = (pos, text)
	| otherwise = (pos + 1, text)
	
type BufferI = (String, String)

emptyI :: BufferI
emptyI = ([], [])

insertI :: Char -> BufferI -> BufferI
insertI c (front, back) = (c:front, back)

deleteI :: BufferI -> BufferI
deleteI ([], back) = ([], back)
deleteI (front, back) = (tail front, back)

atLeftI :: BufferI -> Bool
atLeftI (front, back) = front == []

atRightI :: BufferI -> Bool
atRightI (front, back) = back == []

leftI :: BufferI -> BufferI
leftI (front, back)
	| atLeftI (front, back) = (front, back)
	| otherwise = (tail front, (head front):back)
	
rightI :: BufferI -> BufferI
rightI (front, back)
	| atRightI (front, back) = (front, back)
	| otherwise = ((head back):front, tail back)
	
retrieve :: BufferI -> Buffer
retrieve (front, back) = (length front, (reverse front) ++ back)

prop_empty :: BufferI -> Bool
prop_empty b = retrieve emptyI == empty

prop_insert :: Char -> BufferI -> Bool
prop_insert c b = retrieve (insertI c b) == insert c (retrieve b)
	
prop_delete :: BufferI -> Bool
prop_delete  b = retrieve (deleteI b) == delete (retrieve b)
	
prop_left :: BufferI -> Bool
prop_left b = retrieve (leftI b) == left (retrieve b)

prop_right :: BufferI -> Bool
prop_right b = retrieve (rightI b) == right (retrieve b)

prop_atLeft :: BufferI -> Bool
prop_atLeft b = atLeftI b == atLeft (retrieve b)

prop_atRight :: BufferI -> Bool
prop_atRight b = atRightI b == atRight (retrieve b)

checkAll = do
	quickCheck prop_empty
	quickCheck prop_insert
	quickCheck prop_delete
	quickCheck prop_left
	quickCheck prop_right
	quickCheck prop_atLeft
	quickCheck prop_atRight
