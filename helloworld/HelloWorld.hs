module HelloWorld where

import Clash.Prelude
import FSM.HD44780

topEntity 
    :: "clk" ::: Clock System
    -> "hd44780" ::: Signal System HD44780_Input
topEntity clk = pure empty_hd44780

