# Hack assembler

This is an implementation of the Hack assembler as described in [Project
6](https://www.nand2tetris.org/project06).

The implementation is a more lenient than the specification in the sense
that the assembler may still translate (perhaps incorrectly) ill-formed programs
rather than printing an error. This is more a bug rather than a feature.

Run the assembler:
```sh
  dune exec ./assembler.exe <input .asm file>
```
