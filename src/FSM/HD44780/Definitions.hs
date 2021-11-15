{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780.Definitions where

import Clash.Prelude

data Data_Input = Data_Input {
    data_rs    :: "rs"    ::: Bool,
    data_rw    :: "rw"    ::: Bool,
    data_e     :: "e"     ::: Bool,
    data_idata :: "idata" ::: BitVector 8
} deriving (Show, Generic, NFDataX)

data Data_Output = Data_Output {
    data_odata :: "odata" :::BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Input = Bus_Input {
    bus_valid :: "valid" ::: Bool,
    bus_rs    :: "rs"    ::: Bool,
    bus_data  :: "data"  ::: BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Output = Bus_Output {
    bus_wait :: "wait" ::: Bool
} deriving (Show, Generic, NFDataX)

empty_data :: Data_Input
empty_data = Data_Input False False False 0

write_data :: Bool -> BitVector 8 -> Bool -> Data_Input
write_data rs d e = Data_Input rs False e d

idle_bus :: Bus_Output
idle_bus = Bus_Output False

busy_bus :: Bus_Output
busy_bus = Bus_Output True

empty_bus :: Bus_Input
empty_bus = Bus_Input False undefined undefined

valid_bus :: Bool -> BitVector 8 -> Bus_Input
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

