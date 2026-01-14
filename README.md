# Pas8088

## What is this?

**Pas8088** is a software implementation of the Intel 8086/8088 CPU written in Free Pascal.

## Contents

- **`src/pas8088`**  
  Core Pascal units and a Lazarus package (`pas8088.lpk`) implementing the CPU functionality.  
  Written entirely in Pascal with no external dependencies beyond the Free Pascal RTL.
  Current state: passes the 8088 V2 test suite from https://github.com/SingleStepTests/8088.
  A few instructions (IDIV, IMUL) may set _unspecified_ flags differently than Intel's microcode does.
  Developed and tested with **Free Pascal 3.3.1**.
