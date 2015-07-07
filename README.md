# FAEC: Functions and Arithmetic Expressions Compiler

This project is a compiler for the FAE language, studied in the cource of programming languages at DCC/uchile, to the MIPS instruction set.

## Syntax

The syntax of the language is a sub-set of the racket syntax. The language implements functions as closures.

```racket
<expr> ::= <num>                        number
         | <id>                         indentifier
         | {+ <expr> <expr>}            arithmetic addition
         | {- <expr> <expr>}            arithmetic subtraction
         | {fun {<id>} <expr>}          function definition
         | {<expr> <expr>}              function application
```

## Usage

The compiler can be used with an instalation of racket by running:

```
racket FAEC.rkt <input-file> <output-file>
```

The output file can be loaded into an instance of SPIM (A MIPS simulator: http://spimsimulator.sourceforge.net/)

```
user@machine spim
(spim) load "<output-file>"
(spim) run

```

## Example file

The following example is a corresponds to a function which receives a function "f" and returns a function that applies "f" to its argument "arg". In the example a function which adds one to its argument is passed as "f" and the actual arithmetic argument is 5.

```
{{{fun {f} 
       {fun {arg} 
            {f arg}}} 
  {fun {x} {+ x 1}}} 5}
```


