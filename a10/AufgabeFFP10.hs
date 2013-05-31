module AufgabeFFP10 where 

type Buffer = (Int,String)
empty   :: Buffer                   -- the empty buffer
insert  :: Char -> Buffer -> Buffer -- insert character before cursor
delete  :: Buffer -> Buffer         -- delete character before cursor
left    :: Buffer -> Buffer         -- move cursor left one character
right   :: Buffer -> Buffer         -- move cursor right one character
atLeft  :: Buffer -> Bool           -- is cursor at left end?
atRight :: Buffer -> Bool           -- is cursor at right end?

type BufferI = (String,String)
emptyI   :: Buffer                   -- the empty buffer
insertI  :: Char -> Buffer -> Buffer -- insert character before cursor
deleteI  :: Buffer -> Buffer         -- delete character before cursor
leftI    :: Buffer -> Buffer         -- move cursor left one character
rightI   :: Buffer -> Buffer         -- move cursor right one character
atLeftI  :: Buffer -> Bool           -- is cursor at left end?
atRightI :: Buffer -> Bool           -- is cursor at right end?

retrieve :: BufferI -> Buffer
