{-# LANGUAGE FlexibleContexts #-}
module FSM.HD44780.Definitions where

import Clash.Prelude

data HD44780_Input = HD44780_Input {
    hd44780_rs    :: "rs"    ::: Bool,
    hd44780_rw    :: "rw"    ::: Bool,
    hd44780_e     :: "e"     ::: Bool,
    hd44780_idata :: "idata" ::: BitVector 8
} deriving (Show, Generic, NFDataX)

data HD44780_Output = HD44780_Output {
    hd44780_odata :: "odata" :::BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Input = Bus_Input {
    bus_valid :: "valid" ::: Bool,
    bus_rs    :: "rs"    ::: Bool,
    bus_data  :: "data"  ::: BitVector 8
} deriving (Show, Generic, NFDataX)

data Bus_Output = Bus_Output {
    bus_wait :: "wait" ::: Bool
} deriving (Show, Generic, NFDataX)

empty_hd44780 :: HD44780_Input
empty_hd44780 = HD44780_Input False False False 0

write_hd44780 :: Bool -> BitVector 8 -> Bool -> HD44780_Input
write_hd44780 rs d e = HD44780_Input rs False e d

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

