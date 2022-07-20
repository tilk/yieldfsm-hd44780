{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Definitions for the HD44780 communication protocol.
-}
module FSM.HD44780.Commands where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

-- | CGRAM\/DDRAM address counter counting direction. Corresponds to the I\/D bit in the datasheet.
data EDir = EDecrement  -- ^ Decrement address after write.
          | EIncrement  -- ^ Increment address after write.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Display shifting after writes to DDRAM. Corresponds to the S bit in the datasheet.
data EShift = ENoShift  -- ^ Don't shift the display after DDRAM write.
            | EShift    -- ^ Shift the display after DDRAM write.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Display state. Corresponds to the D bit in the datasheet.
data DDisplay = DOff    -- ^ Display turned off - nothing is displayed.
              | DOn     -- ^ Display turned on.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Cursor visibility. Corresponds to the C bit in the datasheet.
data DCursor = DNoCursor -- ^ Cursor not visible.
             | DCursor   -- ^ Cursor visible.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Blinking of the character indicated by the cursor.  Corresponds to the B bit in the datasheet.
data DBlink = DNoBlink  -- ^ No blinking.
            | DBlink    -- ^ Blinking.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Shift cursor or display. Corresponds to the S\/C bit in the datasheet.
data SShift = SCursor   -- ^ Shift cursor.
            | SShift    -- ^ Shift display.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Shifting direction. Corresponds to the R\/L bit in the datasheet.
data SDir = SLeft       -- ^ Shift left.
          | SRight      -- ^ Shift right.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Interface data length. Corresponds to the DL bit in the datasheet.
data FLength = F4bit    -- ^ 4-bit interface.
             | F8bit    -- ^ 8-bit interface.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Number of display lines. Corresponds to the N bit in the datasheet.
data FLines = F1line    -- ^ 1 display line.
            | F2lines   -- ^ 2 display lines.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Character font. Corresponds to the F bit in the datasheet.
data FFont = F5x8font   -- ^ 5x8 font.
           | F5x10font  -- ^ 5x10 font.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | HD44780 commands, to be sent to the instruction register (RS=0).
data Command = Clear                           -- ^ Clears entire display and sets DDRAM address 0.
             | Home                            -- ^ Sets DDRAM address 0 and un-shifts the display.
             | EntryMode EDir EShift           -- ^ Sets cursor move direction and specifies display shift.
             | Display DDisplay DCursor DBlink -- ^ Sets display on\/off, cursor on\/off, and blinking of cursor position character.
             | Shift SShift SDir               -- ^ Moves cursor and shifts display without modifying DDRAM.
             | Function FLength FLines FFont   -- ^ Sets interface data length, number of display lines and character font.
             | SetCGRAM (Unsigned 6)           -- ^ Sets CGRAM address. Following reads\/writes are to CGRAM.
             | SetDDRAM (Unsigned 7)           -- ^ Sets DDRAM address. Following reads\/writes are to DDRAM.
{-# ANN module (DataReprAnn 
                $(liftQ [t|Command|])
                8
                [ ConstrRepr 'Clear     0xff 0x01 [],
                  ConstrRepr 'Home      0xfe 0x02 [],
                  ConstrRepr 'EntryMode 0xfc 0x04 [0x02, 0x01],
                  ConstrRepr 'Display   0xf8 0x08 [0x04, 0x02, 0x01],
                  ConstrRepr 'Shift     0xf0 0x10 [0x08, 0x04],
                  ConstrRepr 'Function  0xe0 0x20 [0x10, 0x08, 0x04],
                  ConstrRepr 'SetCGRAM  0xc0 0x40 [0x3f],
                  ConstrRepr 'SetDDRAM  0x80 0x80 [0x7f]
                ]) #-} 
deriveBitPack [t|Command|]


