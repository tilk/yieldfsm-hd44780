{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780(hd44780, module FSM.HD44780.Definitions) where

import FSM
import FSM.HD44780.Definitions
import Clash.Prelude

[fsm|hd44780 :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
             => Signal dom Bus_Input
             -> Signal dom (HD44780_Input, Bus_Output)
input bus
fun sendbyte (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (write_hd44780 rs d False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (write_hd44780 rs d True, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (write_hd44780 rs d False, busy_bus)
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield (empty_hd44780, busy_bus)
-- wait 40 ms
call delay 40000
-- initialize display
call sendbyte (False, 0x30)
call delay 4100
call sendbyte (False, 0x30)
call delay 100
call sendbyte (False, 0x30)
call delay 100
call sendbyte (False, 0x3c)
call delay 53
call sendbyte (False, 0x08)
call delay 53
call sendbyte (False, 0x01)
call delay 3000
call sendbyte (False, 0x07)
call delay 53
-- handle bus requests
forever:
    do:
        yield (empty_hd44780, idle_bus)
    until bus_valid bus'
    yield (empty_hd44780, idle_bus)
    call sendbyte (bus_rs bus', bus_data bus')
    call delay 53
|]

