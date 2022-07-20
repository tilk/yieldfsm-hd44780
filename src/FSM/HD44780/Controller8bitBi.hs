{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

8-bit controller, bidirectional communication.
-}
module FSM.HD44780.Controller8bitBi(controller8bitBi) where

import FSM
import FSM.HD44780.Definitions
import FSM.HD44780.Commands
import Clash.Prelude
import Control.Arrow((***))

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
output d = empty_data :: Data_Input 8
output b = busy_bus
fun sendbyte (rs, d):
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield<d> write_data rs d False
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield<d> write_data rs d True
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield<d> write_data rs d False
fun readbyte rs:
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield<d> read_data rs False
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield<d> read_data rs True
    let d = di
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield<d> read_data rs False
    ret d
fun delay us :: Unsigned (BitsFor 40000):
    repeat1 us:
        repeat1 unsigned_cycles_us (clockPeriod @dom) d1:
            yield
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
        yield<b> idle_bus
    until bus_valid bus'
    call sendWait (bus_rs bus', bus_data bus')
|]

