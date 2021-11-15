module FSM.HD44780.Commands where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

data EDir = EDecrement | EIncrement
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data EShift = ENoShift | EShift
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data DDisplay = DOff | DOn
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data DCursor = DNoCursor | DCursor
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data DBlink = DNoBlink | DBlink
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data SShift = SCursor | SShift
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data SDir = SLeft | SRight
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data FLength = F4bit | F8bit
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data FLines = F1line | F2lines
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data FFont = F5x8font | F5x10font
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data Command = Clear
             | Home
             | EntryMode EDir EShift
             | Display DDisplay DCursor DBlink
             | Shift SShift SDir
             | Function FLength FLines FFont
             | SetCGRAM (Unsigned 6)
             | SetDDRAM (Unsigned 7)
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


