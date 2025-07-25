# Pas8088

## What is this?

**Pas8088** is a software implementation of the Intel 8086/8088 CPU written in Free Pascal.

## Contents

- **`src/pas8088`**  
  Core Pascal units and a Lazarus package (`pas8088.lpk`) implementing the CPU functionality.  
  Written entirely in Pascal with no external dependencies beyond the Free Pascal RTL.
  Current state: 100% of the CPU instructions are delivered to the state that allows running DOS, as well as many of the real mode software from that era. Still likely to contain errors, programs may crash.  
  Developed and tested with **Free Pascal 3.3.1**.

- **`src/machines/poisk`**  
  Emulator of the [Poisk](https://en.wikipedia.org/wiki/Poisk_%28computer%29) — an i8088-based IBM PC compatible computer, designed and manufactured by PO Electronmash (Kyiv, Ukraine) in the late 1980s to early 1990s.  
  Built on top of `pas8088`, it uses the RayLib-based [ray4laz](https://github.com/GuvaCode/ray4laz) package for graphics and audio.
  Developed and tested with **Free Pascal 3.3.1**.
