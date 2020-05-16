# Virtual machine translator

This is an implementation of the Jack compiler as described in [Project
10](https://www.nand2tetris.org/project10) and [Project
11](https://www.nand2tetris.org/project11).

Run the compiler:
```sh
  dune exec ./jack_compiler.exe <input file or directory>
```

The input must either be a .jack file or a directory holding .jack files at the top
level. The output will be  .vm file in the same directory as the input.
