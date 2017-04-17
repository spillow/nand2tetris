// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

// R2 = R0 * R1

// cnt = R1
  @R1
  D=M
  @cnt
  M=D
// sum = 0
  @sum
  M=0
// LOOP
  (LOOP)
// IF cnt == 0 goto END
  @cnt
  D=M
  @END
  D;JEQ
// sum = sum + R0
  @sum
  D=M
  @R0
  D=D+M
  @sum
  M=D
// cnt = cnt - 1
  @cnt
  M=M-1
// ENDLOOP
  @LOOP
  0;JMP

// END
  (END)
// R2 = sum
  @sum
  D=M
  @R2
  M=D
// while(1)
  @HALT
  (HALT)
  0;JMP
