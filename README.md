# yieldfsm-hd44780

[YieldFSM](https://github.com/tilk/yieldfsm) is a proof of concept DSL for [Clash](https://clash-lang.org/) designed for specifying (Mealy)
finite state machines using imperative, procedural code.
This repository contains a family of controllers for the popular HD44780 character displays.
The controller variants differ on:

* Bus width for the display (4-bit or 8-bit),
* Polling of the busy bit or worst-case delays.

The controllers present a simple bus with ready/busy signals, which can be easily adapted to Avalon, Wishbone or other standard buses used on FPGA.

