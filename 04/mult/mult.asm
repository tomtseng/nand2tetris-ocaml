// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Clear R2.
@2
M=0

(LOOP)
// Repeat this loop body R0 many times by decrementing R0 and breaking out of
// the loop when R0 becomes negative.
@0
M=M-1;
D=M
@END
D;JLT

// Add R1 to R2.
@1
D=M
@2
M=M+D

@LOOP
0;JMP

(END)
