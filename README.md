# yieldfsm-hd44780

[YieldFSM](https://github.com/tilk/yieldfsm) is a proof of concept DSL for [Clash](https://clash-lang.org/) designed for specifying (Mealy)
finite state machines using imperative, procedural code.
This repository contains a family of controllers for the popular HD44780 character displays.
The controller variants differ on:

* Bus width for the display (4-bit or 8-bit),
* Polling of the busy bit or worst-case delays.

The controllers present a simple bus with ready/busy signals, which can be easily adapted to Avalon, Wishbone or other standard buses used on FPGA.

# Building and running

Building is done using `stack` (use `stack build`).

A "hello world" example can be built using Shake (use `stack run`). The built cores (as Verilog files) can be found in the `_build` subdirectory, which can then be, for example, synthesized to an FPGA using any toolchain of choice.
