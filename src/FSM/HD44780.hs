{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780(controller4bit, controller8bit, module FSM.HD44780.Definitions) where

import FSM
import FSM.HD44780.Definitions
import FSM.HD44780.Commands
import Clash.Prelude

[fsm|controller8bit :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                    => Signal dom Bus_Input
                    -> Signal dom (Data_Input 8, Bus_Output)
input bus
fun sendbyte (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (write_data rs d False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (write_data rs d True, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (write_data rs d False, busy_bus)
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield (empty_data, busy_bus)
fun sendDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendbyte (rs, d)
    call delay (zeroExtend us)
-- wait 40 ms
call delay 40000
-- initialize display
call sendDelay (4100, False, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  False, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  False, pack $ Function F8bit F1line F5x8font)
call sendDelay (53,   False, pack $ Function F8bit F2lines F5x8font)
call sendDelay (53,   False, pack $ Display DOff DNoCursor DNoBlink)
call sendDelay (3000, False, pack $ Clear)
call sendDelay (53,   False, pack $ EntryMode EIncrement ENoShift)
call sendDelay (53,   False, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendDelay (53, bus_rs bus', bus_data bus')
|]

[fsm|controller4bit :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                    => Signal dom Bus_Input
                    -> Signal dom (Data_Input 4, Bus_Output)
input bus
fun sendnibble (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (write_data rs d False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (write_data rs d True, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (write_data rs d False, busy_bus)
fun sendbyte (rs, d):
    let p = unpack d
    call sendnibble (rs, fst p)
    call sendnibble (rs, snd p)
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield (empty_data, busy_bus)
fun sendNibbleDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendnibble (rs, d)
    call delay (zeroExtend us)
-- wait 40 ms
fun sendDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendbyte (rs, d)
    call delay (zeroExtend us)
-- wait 40 ms
call delay 40000
-- initialize display
call sendNibbleDelay (4100, False, 0x03)
call sendNibbleDelay (100,  False, 0x03)
call sendNibbleDelay (100,  False, 0x03)
call sendNibbleDelay (100,  False, 0x02)
call sendDelay (53,   False, pack $ Function F4bit F2lines F5x8font)
call sendDelay (53,   False, pack $ Display DOff DNoCursor DNoBlink)
call sendDelay (3000, False, pack $ Clear)
call sendDelay (53,   False, pack $ EntryMode EIncrement ENoShift)
call sendDelay (53,   False, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendDelay (53, bus_rs bus', bus_data bus')
|]

