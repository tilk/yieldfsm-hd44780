module FSM.HD44780.Definitions where

import Clash.Prelude

data HD44780_Input = HD44780_Input {
    hd44780_rs :: Bool,
    hd44780_rw :: Bool,
    hd44780_e :: Bool,
    hd44780_idata :: BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Input = Bus_Input {
    bus_valid :: Bool,
    bus_rs :: Bool,
    bus_data :: BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Output = Bus_Output {
    bus_wait :: Bool
} deriving (Show, Generic, NFDataX)

empty_hd44780 :: HD44780_Input
empty_hd44780 = HD44780_Input False False False 0

write_hd44780 :: Bool -> BitVector 8 -> Bool -> HD44780_Input
write_hd44780 rs d e = HD44780_Input rs False e d

idle_bus :: Bus_Output
idle_bus = Bus_Output False

busy_bus :: Bus_Output
busy_bus = Bus_Output True

