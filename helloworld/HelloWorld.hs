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
call send (True, fromIntegral $ fromEnum ' ')
call send (True, fromIntegral $ fromEnum 'w')
call send (True, fromIntegral $ fromEnum 'o')
call send (True, fromIntegral $ fromEnum 'r')
call send (True, fromIntegral $ fromEnum 'l')
call send (True, fromIntegral $ fromEnum 'd')
call send (True, fromIntegral $ fromEnum '!')
forever:
    yield empty_bus
|]

helloWorld8bitCircuit :: HiddenClockResetEnable ExampleSystem
                      => Signal ExampleSystem HD44780_Input
helloWorld8bitCircuit = fst <$> sig where
    sig = hd44780 $ helloWorld $ snd <$> sig

helloWorld8bit 
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780" ::: Signal ExampleSystem HD44780_Input
helloWorld8bit clk rst = exposeClockResetEnable helloWorld8bitCircuit clk rst enableGen

makeTopEntity 'helloWorld8bit

