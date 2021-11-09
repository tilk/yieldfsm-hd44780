module HelloWorld where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.HD44780
import FSM

[fsm|helloWorld :: forall dom. HiddenClockResetEnable dom
                => Signal dom Bus_Output
                -> Signal dom Bus_Input
input bus
fun send(rs, d):
    do:
        yield valid_bus rs d
    while bus_wait bus'
call send (True, fromIntegral $ fromEnum 'H')
call send (True, fromIntegral $ fromEnum 'e')
call send (True, fromIntegral $ fromEnum 'l')
call send (True, fromIntegral $ fromEnum 'l')
call send (True, fromIntegral $ fromEnum 'o')
forever:
    yield empty_bus
|]

topEntity 
    :: "clk" ::: Clock System
    -> "rst" ::: Reset System
    -> "hd44780" ::: Signal System HD44780_Input
topEntity clk rst = exposeClockResetEnable (fst <$> hd44780 (pure $ Bus_Input False undefined undefined)) clk rst enableGen

makeTopEntity 'topEntity

