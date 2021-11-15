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
fun sendData d:
    ret call send(True, d)
call sendData (fromIntegral $ fromEnum 'H')
call sendData (fromIntegral $ fromEnum 'e')
call sendData (fromIntegral $ fromEnum 'l')
call sendData (fromIntegral $ fromEnum 'l')
call sendData (fromIntegral $ fromEnum 'o')
call sendData (fromIntegral $ fromEnum ' ')
call sendData (fromIntegral $ fromEnum 'w')
call sendData (fromIntegral $ fromEnum 'o')
call sendData (fromIntegral $ fromEnum 'r')
call sendData (fromIntegral $ fromEnum 'l')
call sendData (fromIntegral $ fromEnum 'd')
call sendData (fromIntegral $ fromEnum '!')
forever:
    yield empty_bus
|]

helloWorld8bitCircuit :: HiddenClockResetEnable ExampleSystem
                      => Signal ExampleSystem Data_Input
helloWorld8bitCircuit = fst <$> sig where
    sig = controller $ helloWorld $ snd <$> sig

helloWorld8bit 
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780" ::: Signal ExampleSystem Data_Input
helloWorld8bit clk rst = exposeClockResetEnable helloWorld8bitCircuit clk rst enableGen

makeTopEntity 'helloWorld8bit

