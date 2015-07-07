# FAEC: Functions and Arithmetic Expressions Compiler

This project is a compiler for the FAE language, studied in the cource of programming languages at DCC/uchile, to the MIPS instruction set.

## Syntax

## Usage

The compiler can be used with an instalation of racket by running:

```
racket FAEC.rkt <input-file> <output-file>
```

The output file can be loaded into an instance of SPIM (A MIPS simulator: http://spimsimulator.sourceforge.net/)

```
user@machine spim

```

```
(spim) load "<output-file>"
(spim) run

```


