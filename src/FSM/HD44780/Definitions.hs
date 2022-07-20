{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Communication interface definitions.
-}
{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780.Definitions where

import Clash.Prelude

-- | Register select. Corresponds to the RS pin.
data RSFlag = Instr -- ^ Read/write the instruction register.
            | Data  -- ^ Read/write the data register.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | Read\/write flag. Corresponds to the R\/W pin.
data RWFlag = Write -- ^ Write register.
            | Read  -- ^ Read register.
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

-- | One-bit flags in the HD44780 hardware interface.
data Data_Flags = Data_Flags {
    data_rs    :: "rs"    ::: RSFlag, -- ^ RS pin.
    data_rw    :: "rw"    ::: RWFlag, -- ^ R\/W pin.
    data_e     :: "e"     ::: Bool    -- ^ E pin.
} deriving (Show, Generic, NFDataX)

-- | HD44780 communication interface. Valid bit widths are 4 or 8.
type Data_Input n = ("flags" ::: Data_Flags, "data" ::: BitVector n)

-- | Communication bus to the YieldFSM controller - input side. Used to send instructions or data to the display.
data Bus_Input = Bus_Input {
    bus_valid :: "valid" ::: Bool,       -- ^ Data is valid for this cycle.
    bus_rs    :: "rs"    ::: RSFlag,     -- ^ RS pin value - instruction or data.
    bus_data  :: "data"  ::: BitVector 8 -- ^ Instruction or data to be sent to the display.
} deriving (Show, Generic, NFDataX)

-- | Communication bus to the YieldFSM controller - output side.
data Bus_Output = Bus_Output {
    bus_wait :: "wait" ::: Bool -- ^ If true, interface not ready to accept commands.
} deriving (Show, Generic, NFDataX)

-- | Idle state of the HD44780 interface.
empty_data :: KnownNat n => Data_Input n
empty_data = (Data_Flags Instr Write False, 0)

-- | Send data to the HD44780. The @e@ parameter needs to be strobed to generate clock edges.
write_data :: KnownNat n => RSFlag -> BitVector n -> Bool -> Data_Input n
write_data rs d e = (Data_Flags rs Write e, d)

-- | Read data from the HD44780. The @e@ parameter needs to be strobed to generate clock edges.
read_data :: KnownNat n => RSFlag -> Bool -> Data_Input n
read_data rs e = (Data_Flags rs Read e, undefined)

-- | Idle state of the controller bus. The controller is accepting commands.
idle_bus :: Bus_Output
idle_bus = Bus_Output False

-- | Busy state of the controller bus. The controller is not accepting commands, waiting is necessary.
busy_bus :: Bus_Output
busy_bus = Bus_Output True

-- | Don't send any command to the controller.
empty_bus :: Bus_Input
empty_bus = Bus_Input False undefined undefined

-- | Send a command to the controller. The bus needs to be not busy.
valid_bus :: RSFlag -> BitVector 8 -> Bus_Input
valid_bus rs d = Bus_Input True rs d

-- | How many bits are needed to represent the number @k@.
type BitsFor k = CLog 2 (k+1)

-- | Type-level division on 'SNat'.
divRUSNat :: (1 <= b) => SNat a -> SNat b -> SNat (DivRU a b)
divRUSNat SNat SNat = SNat

-- | 'Unsigned' from a statically known number.
unsigned :: forall k. KnownNat k => Unsigned (BitsFor k)
unsigned = natToNum @k

-- | 'Unsigned' from a statically known number (as 'SNat').
unsignedSNat :: KnownNat k => SNat k -> Unsigned (BitsFor k)
unsignedSNat n = snatToNum n

-- | Statically calculate the number of cycles for a given time, in picoseconds, given a clock period.
cycles_ps :: (1 <= period) => SNat period -> SNat k -> SNat (k `DivRU` period)
cycles_ps SNat SNat = SNat

-- | Statically calculate the number of cycles for a given time, in picoseconds, given a clock period (using 'SNat').
unsigned_cycles_ps :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor (k `DivRU` period))
unsigned_cycles_ps period k = snatToNum $ cycles_ps period k

-- | Statically calculate the number of cycles for a given time, in nanoseconds, given a clock period (using 'SNat').
unsigned_cycles_ns :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000) `DivRU` period))
unsigned_cycles_ns period k = unsigned_cycles_ps period (mulSNat k (SNat @1000))

-- | Statically calculate the number of cycles for a given time, in microseconds, given a clock period (using 'SNat').
unsigned_cycles_us :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000000) `DivRU` period))
unsigned_cycles_us period k = unsigned_cycles_ps period (mulSNat k (SNat @1000000))

-- | Statically calculate the number of cycles for a given time, in milliseconds, given a clock period (using 'SNat').
unsigned_cycles_ms :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000000000) `DivRU` period))
unsigned_cycles_ms period k = unsigned_cycles_ps period (mulSNat k (SNat @1000000000))

