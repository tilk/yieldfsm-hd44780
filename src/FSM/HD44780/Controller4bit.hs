{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

4-bit controller, unidirectional communication.
-}
module FSM.HD44780.Controller4bit(controller4bit) where

import FSM
import FSM.HD44780.Definitions
import FSM.HD44780.Commands
import Clash.Prelude

[fsm|controller4bit :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                    => Signal dom Bus_Input
                    -> Signal dom (Data_Input 4, Bus_Output)
input bus
output d = empty_data :: Data_Input 4
output b = busy_bus
fun sendnibble (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield<d> write_data rs d False
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield<d> write_data rs d True
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield<d> write_data rs d False
fun sendbyte (rs, d):
    let p = unpack d
    call sendnibble (rs, fst p)
    call sendnibble (rs, snd p)
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield
fun sendNibbleDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendnibble (rs, d)
    call delay (zeroExtend us)
fun sendDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendbyte (rs, d)
    call delay (zeroExtend us)
-- wait 40 ms
call delay 40000
-- initialize display
call sendNibbleDelay (4100, Instr, 0x03)
call sendNibbleDelay (100,  Instr, 0x03)
call sendNibbleDelay (100,  Instr, 0x03)
call sendNibbleDelay (100,  Instr, 0x02)
call sendDelay (53,   Instr, pack $ Function F4bit F2lines F5x8font)
call sendDelay (53,   Instr, pack $ Display DOff DNoCursor DNoBlink)
call sendDelay (3000, Instr, pack $ Clear)
call sendDelay (53,   Instr, pack $ EntryMode EIncrement ENoShift)
call sendDelay (53,   Instr, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield<b> idle_bus
    until bus_valid bus'
    call sendDelay (53, bus_rs bus', bus_data bus')
|]

