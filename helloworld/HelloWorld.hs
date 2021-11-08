module HelloWorld where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.HD44780

topEntity 
    :: "clk" ::: Clock System
    -> "rst" ::: Reset System
    -> "hd44780" ::: Signal System HD44780_Input
topEntity clk rst = exposeClockResetEnable (fst <$> hd44780 (pure $ Bus_Input False undefined undefined)) clk rst enableGen

makeTopEntity 'topEntity

