module FSM.HD44780.Controller4bitBi(controller4bitBi) where

import FSM
import FSM.HD44780.Definitions
import FSM.HD44780.Commands
import Clash.Prelude
import Control.Arrow((***))

mkMaybe :: RWFlag -> a -> Maybe a
mkMaybe Write x = Just x
mkMaybe Read  _ = Nothing

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
fun readnibble rs:
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d50:
        yield<d> read_data rs False
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d250:
        yield<d> read_data rs True
    let d = di
    repeat1 unsigned_cycles_ns (clockPeriod @dom) d200:
        yield<d> read_data rs False
    ret d
fun readbyte rs:
    let dh = call readnibble rs
    let dl = call readnibble rs
    ret pack (dh, dl)
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
        yield<b> idle_bus
    until bus_valid bus'
    call sendWait (bus_rs bus', bus_data bus')
|]

