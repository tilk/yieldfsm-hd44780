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
                      => Signal ExampleSystem (Data_Input 8)
helloWorld8bitCircuit = fst <$> sig where
    sig = controller8bit $ helloWorld $ snd <$> sig

helloWorld8bit
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780" ::: Signal ExampleSystem (Data_Input 8)
helloWorld8bit clk rst = exposeClockResetEnable helloWorld8bitCircuit clk rst enableGen

makeTopEntity 'helloWorld8bit

helloWorld4bitCircuit :: HiddenClockResetEnable ExampleSystem
                      => Signal ExampleSystem (Data_Input 4)
helloWorld4bitCircuit = fst <$> sig where
    sig = controller4bit $ helloWorld $ snd <$> sig

helloWorld4bit
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780" ::: Signal ExampleSystem (Data_Input 4)
helloWorld4bit clk rst = exposeClockResetEnable helloWorld4bitCircuit clk rst enableGen

makeTopEntity 'helloWorld4bit

helloWorld8bitBiCircuit :: HiddenClockResetEnable ExampleSystem
                        => BiSignalIn 'Floating ExampleSystem 8
                        -> (Signal ExampleSystem Data_Flags, BiSignalOut 'Floating ExampleSystem 8)
helloWorld8bitBiCircuit d = (f, di) where
    (f, di, bo) = controller8bitBi (d, helloWorld bo)

helloWorld8bitBi
    :: "clk" ::: Clock ExampleSystem
    -> "rst" ::: Reset ExampleSystem
    -> "hd44780_data" ::: BiSignalIn 'Floating ExampleSystem 8
    -> "hd44780" ::: ("flags" ::: Signal ExampleSystem Data_Flags, BiSignalOut 'Floating ExampleSystem 8)
helloWorld8bitBi clk rst = exposeClockResetEnable helloWorld8bitBiCircuit clk rst enableGen

makeTopEntity 'helloWorld8bitBi

