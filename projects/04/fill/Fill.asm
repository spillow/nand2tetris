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

// Put your code here.

// (-1 = black, 0 = white)

// R0 is pixel value (0 or -1)

// LOOP
  (LOOP)
// if kbd != 0 goto BLACK
  @KBD
  D=M
  @BLACK
  D;JNE
//   R0 = 0 (white)
  @R0
  M=0
//   goto WRITESCREEN
  @WRITESCREEN
  0;JMP
//
// BLACK
  (BLACK)
// R0 = -1 (black)
  @R0
  M=-1
// goto WRITESCREEN
  @WRITESCREEN
  0;JMP
//
// WRITESCREEN
  (WRITESCREEN)
// cnt = 8192
  @8192
  D=A
  @cnt
  M=D
// SLOOP
  (SLOOP)
// if cnt == 0 goto LOOP
  @cnt
  D=M;
  @LOOP
  D;JEQ
// cnt = cnt - 1
  @cnt
  M=M-1
// SCREEN[cnt] = R0
  @cnt
  D=M
  @SCREEN
  D=A+D
  @SADDR
  M=D
  @R0
  D=M
  @SADDR
  A=M
  M=D

// goto SLOOP
  @SLOOP
  0;JMP
