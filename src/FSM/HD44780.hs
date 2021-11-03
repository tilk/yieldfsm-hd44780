module FSM.HD44780 where

import FSM
import FSM.HD44780.Definitions
import Clash.Prelude

[fsm|hd44780 :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period)
             => Signal dom Bus_Input
             -> Signal dom (HD44780_Input, Bus_Output)
input bus
fun sendbyte (rs, d):
    repeat (1 :: Unsigned (Log2 period)):
        yield (write_hd44780 rs d False, busy_bus)
    yield (write_hd44780 rs d True, busy_bus)
forever:
    while not (bus_valid bus):
        yield (empty_hd44780, idle_bus)
    yield (empty_hd44780, idle_bus)
    call sendbyte (bus_rs bus', bus_data bus')
|]

