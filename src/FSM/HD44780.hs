{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780(
    controller4bit, controller8bit,
    controller4bitBi, controller8bitBi,
    controller4bitBF, controller8bitBF,
    module FSM.HD44780.Definitions
) where

import FSM
import FSM.HD44780.Definitions
import FSM.HD44780.Commands
import Clash.Prelude
import Control.Arrow((***))

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
call sendDelay (4100, Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (53,   Instr, pack $ Function F8bit F2lines F5x8font)
call sendDelay (53,   Instr, pack $ Display DOff DNoCursor DNoBlink)
call sendDelay (3000, Instr, pack $ Clear)
call sendDelay (53,   Instr, pack $ EntryMode EIncrement ENoShift)
call sendDelay (53,   Instr, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendDelay (53, bus_rs bus', bus_data bus')
|]

mkMaybe :: RWFlag -> a -> Maybe a
mkMaybe Write x = Just x
mkMaybe Read  _ = Nothing

controller8bitBi :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                 => (BiSignalIn 'Floating dom 8, Signal dom Bus_Input)
                 -> (Signal dom Data_Flags, BiSignalOut 'Floating dom 8, Signal dom Bus_Output)
controller8bitBi (bd, bi) = (df, writeToBiSignal bd d', bo)
    where
    ((df, d), bo) = (unbundle *** id) . unbundle . controller8bitBF . bundle $ (readFromBiSignal bd, bi)
    d' = mkMaybe <$> (data_rw <$> df) <*> d

[fsm|controller8bitBF :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                      => Signal dom (BitVector 8, Bus_Input)
                      -> Signal dom (Data_Input 8, Bus_Output)
input (di, bus)
fun sendbyte (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (write_data rs d False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (write_data rs d True, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (write_data rs d False, busy_bus)
fun readbyte rs:
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (read_data rs False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (read_data rs True, busy_bus)
    let d = di
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (read_data rs False, busy_bus)
    ret d
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield (empty_data, busy_bus)
fun sendDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendbyte (rs, d)
    call delay (zeroExtend us)
fun wait ():
    var b = undefined
    do:
        b = call readbyte Instr
    while testBit b 7
fun sendWait (rs, d):
    call sendbyte (rs, d)
    call wait ()
    call delay 1
-- wait 40 ms
call delay 40000
-- initialize display
call sendDelay (4100, Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (100,  Instr, pack $ Function F8bit F1line F5x8font)
call sendDelay (53,   Instr, pack $ Function F8bit F2lines F5x8font)
call sendWait (Instr, pack $ Display DOff DNoCursor DNoBlink)
call sendWait (Instr, pack $ Clear)
call sendWait (Instr, pack $ EntryMode EIncrement ENoShift)
call sendWait (Instr, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendWait (bus_rs bus', bus_data bus')
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
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendDelay (53, bus_rs bus', bus_data bus')
|]

controller4bitBi :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                 => (BiSignalIn 'Floating dom 4, Signal dom Bus_Input)
                 -> (Signal dom Data_Flags, BiSignalOut 'Floating dom 4, Signal dom Bus_Output)
controller4bitBi (bd, bi) = (df, writeToBiSignal bd d', bo)
    where
    ((df, d), bo) = (unbundle *** id) . unbundle . controller4bitBF . bundle $ (readFromBiSignal bd, bi)
    d' = mkMaybe <$> (data_rw <$> df) <*> d

[fsm|controller4bitBF :: forall dom period. (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period)
                      => Signal dom (BitVector 4, Bus_Input)
                      -> Signal dom (Data_Input 4, Bus_Output)
input (di, bus)
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
fun readnibble rs:
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield (read_data rs False, busy_bus)
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield (read_data rs True, busy_bus)
    let d = di
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield (read_data rs False, busy_bus)
    ret d
fun readbyte rs:
    let dh = call readnibble rs
    let dl = call readnibble rs
    ret pack (dh, dl)
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield (empty_data, busy_bus)
fun sendNibbleDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendnibble (rs, d)
    call delay (zeroExtend us)
fun sendDelay (us :: Unsigned (BitsFor 4100), rs, d):
    call sendbyte (rs, d)
    call delay (zeroExtend us)
fun wait ():
    var b = undefined
    do:
        b = call readbyte Instr
    while testBit b 7
fun sendWait (rs, d):
    call sendbyte (rs, d)
    call wait ()
    call delay 1
-- wait 40 ms
call delay 40000
-- initialize display
call sendNibbleDelay (4100, Instr, 0x3)
call sendNibbleDelay (100,  Instr, 0x3)
call sendNibbleDelay (100,  Instr, 0x3)
call sendNibbleDelay (100,  Instr, 0x2)
call sendWait (Instr, pack $ Display DOff DNoCursor DNoBlink)
call sendWait (Instr, pack $ Clear)
call sendWait (Instr, pack $ EntryMode EIncrement ENoShift)
call sendWait (Instr, pack $ Display DOn DNoCursor DNoBlink)
-- handle bus requests
forever:
    do:
        yield (empty_data, idle_bus)
    until bus_valid bus'
    call sendWait (bus_rs bus', bus_data bus')
|]

