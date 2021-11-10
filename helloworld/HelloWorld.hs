module HelloWorld where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.HD44780
import FSM

createDomain (knownVDomain @XilinxSystem) {vName="ExampleSystem", vPeriod=30030}

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

topCircuit :: HiddenClockResetEnable ExampleSystem
           => Signal ExampleSystem HD44780_Input
topCircuit = fst <$> sig where
    sig = hd44780 $ helloWorld $ snd <$> sig

topEntity 
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780" ::: Signal ExampleSystem HD44780_Input
topEntity clk rst = exposeClockResetEnable topCircuit clk rst enableGen

makeTopEntity 'topEntity

