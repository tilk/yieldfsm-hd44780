{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780.Definitions where

import Clash.Prelude

data RSFlag = Instr | Data
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data RWFlag = Write | Read
    deriving (Show, Eq, Ord, Enum, Generic, NFDataX, BitPack)

data Data_Flags = Data_Flags {
    data_rs    :: "rs"    ::: RSFlag,
    data_rw    :: "rw"    ::: RWFlag,
    data_e     :: "e"     ::: Bool
} deriving (Show, Generic, NFDataX)

type Data_Input n = ("flags" ::: Data_Flags, "data" ::: BitVector n)

data Bus_Input = Bus_Input {
    bus_valid :: "valid" ::: Bool,
    bus_rs    :: "rs"    ::: RSFlag,
    bus_data  :: "data"  ::: BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Output = Bus_Output {
    bus_wait :: "wait" ::: Bool
} deriving (Show, Generic, NFDataX)

empty_data :: KnownNat n => Data_Input n
empty_data = (Data_Flags Instr Write False, 0)

write_data :: KnownNat n => RSFlag -> BitVector n -> Bool -> Data_Input n
write_data rs d e = (Data_Flags rs Write e, d)

read_data :: KnownNat n => RSFlag -> Bool -> Data_Input n
read_data rs e = (Data_Flags rs Read e, undefined)

idle_bus :: Bus_Output
idle_bus = Bus_Output False

busy_bus :: Bus_Output
busy_bus = Bus_Output True

empty_bus :: Bus_Input
empty_bus = Bus_Input False undefined undefined

valid_bus :: RSFlag -> BitVector 8 -> Bus_Input
valid_bus rs d = Bus_Input True rs d

type BitsFor k = CLog 2 (k+1)

divRUSNat :: (1 <= b) => SNat a -> SNat b -> SNat (DivRU a b)
divRUSNat SNat SNat = SNat

unsigned :: forall k. KnownNat k => Unsigned (BitsFor k)
unsigned = natToNum @k

unsignedSNat :: KnownNat k => SNat k -> Unsigned (BitsFor k)
unsignedSNat n = snatToNum n

cycles_ps :: (1 <= period) => SNat period -> SNat k -> SNat (k `DivRU` period)
cycles_ps SNat SNat = SNat

unsigned_cycles_ps :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor (k `DivRU` period))
unsigned_cycles_ps period k = snatToNum $ cycles_ps period k

unsigned_cycles_ns :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000) `DivRU` period))
unsigned_cycles_ns period k = unsigned_cycles_ps period (mulSNat k (SNat @1000))

unsigned_cycles_us :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000000) `DivRU` period))
unsigned_cycles_us period k = unsigned_cycles_ps period (mulSNat k (SNat @1000000))

unsigned_cycles_ms :: (1 <= period, KnownNat period, KnownNat k) => SNat period -> SNat k -> Unsigned (BitsFor ((k * 1000000000) `DivRU` period))
unsigned_cycles_ms period k = unsigned_cycles_ps period (mulSNat k (SNat @1000000000))

