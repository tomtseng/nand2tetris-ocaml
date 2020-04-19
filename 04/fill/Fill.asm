// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(RESET_CURRENT_SCREEN_WORD)
@SCREEN
D=A
@current_screen_word
M=D

// Repeatedly write to `current_screen_word` based on keyboard input and
// increment `current_screen_word`.
(LOOP)

@KBD
D=M
@WRITE_BLACK_WORD
D;JNE

(WRITE_WHITE_WORD)
@current_screen_word
A=M
M=0
@END_WRITE
0;JMP

(WRITE_BLACK_WORD)
@current_screen_word
A=M
M=-1

(END_WRITE)

@current_screen_word
M=M+1
D=M
@24576  // last screen word
D=A-D;
@RESET_CURRENT_SCREEN_WORD
D;JLE
@LOOP
0;JMP
