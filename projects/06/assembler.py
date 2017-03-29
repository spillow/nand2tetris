#!/usr/bin/env python
from pyparsing import *
import argparse
import os
import sys

def stringbin(x):
    return format(x, '016b')

class BranchTarget:
    def __init__(self, toks):
        self.label = toks[0]

    def __str__(self):
        return '(' + self.label + ')'

    def __repr__(self):
        return self.__str__()

class AInst:
    def __init__(self, value):
        self.value = value # int or string

    def __str__(self):
        return '@' + str(self.value)

    def __repr__(self):
        return self.__str__()

    def emit(self, symtab):
        if self.value in symtab:
            val = symtab[self.value]
        else:
            val = self.value

        return stringbin(val)

class CInst:
    def __init__(self, toks):
        self.dest = None
        self.comp = None
        self.jump = None
        if len(toks) == 1: # just comp
            self.comp = toks[0]
        elif len(toks) == 2:
            if toks[1][0] == 'J':
                self.comp = toks[0]
                self.jump = toks[1]
            else:
                self.dest = toks[0]
                self.comp = toks[1]
        elif len(toks) == 3:
            self.dest = toks[0]
            self.comp = toks[1]
            self.jump = toks[2]
        else:
            assert False

    def __str__(self):
        out = ""
        if self.dest:
            out += "%s=" % (self.dest)
        out += "%s" % (self.comp)
        if self.jump:
            out += ";%s" % (self.jump)

        return out

    def __repr__(self):
        return self.__str__()

    def __comp(self):
        if   self.comp == '0':   return 0b0101010
        elif self.comp == '1':   return 0b0111111
        elif self.comp == '-1':  return 0b0111010
        elif self.comp == 'D':   return 0b0001100
        elif self.comp == 'A':   return 0b0110000
        elif self.comp == '!D':  return 0b0001101
        elif self.comp == '!A':  return 0b0110001
        elif self.comp == '-D':  return 0b0001111
        elif self.comp == '-A':  return 0b0110011
        elif self.comp == 'D+1': return 0b0011111
        elif self.comp == 'A+1': return 0b0110111
        elif self.comp == 'D-1': return 0b0001110
        elif self.comp == 'A-1': return 0b0110010
        elif self.comp == 'D+A': return 0b0000010
        elif self.comp == 'D-A': return 0b0010011
        elif self.comp == 'A-D': return 0b0000111
        elif self.comp == 'D&A': return 0b0000000
        elif self.comp == 'D|A': return 0b0010101

        elif self.comp == 'M':   return 0b1110000
        elif self.comp == '!M':  return 0b1110001
        elif self.comp == '-M':  return 0b1110011
        elif self.comp == 'M+1': return 0b1110111
        elif self.comp == 'M-1': return 0b1110010
        elif self.comp == 'D+M': return 0b1000010
        elif self.comp == 'D-M': return 0b1010011
        elif self.comp == 'M-D': return 0b1000111
        elif self.comp == 'D&M': return 0b1000000
        elif self.comp == 'D|M': return 0b1010101

        else:
            assert False, "unknown computation"

    def __dest(self):
        if self.dest is None:
            return 0b000

        val = 0

        if 'M' in self.dest:
            val |= (1 << 0)

        if 'D' in self.dest:
            val |= (1 << 1)

        if 'A' in self.dest:
            val |= (1 << 2)

        return val

    def __jump(self):
        if self.jump is None:
            return 0b000

        if   self.jump == 'JGT': return 0b001
        elif self.jump == 'JEQ': return 0b010
        elif self.jump == 'JGE': return 0b011
        elif self.jump == 'JLT': return 0b100
        elif self.jump == 'JNE': return 0b101
        elif self.jump == 'JLE': return 0b110
        elif self.jump == 'JMP': return 0b111

        else:
            assert False, "unknown jump"

    def emit(self, symtab):
        comp = self.__comp()
        dest = self.__dest()
        jump = self.__jump()

        val = (0x7 << 13) | (comp << 6) | (dest << 3) | (jump << 0)
        return stringbin(val)

def mkAInst(toks):
    return AInst(toks[0][0])

def mkCInst(toks):
    return CInst(toks[0])

def mkBranchTarget(toks):
    return BranchTarget(toks[0])

def parse(asm):
    ParserElement.setDefaultWhitespaceChars(' \t')

    lparen = Literal('(').suppress()
    rparen = Literal(')').suppress()
    atsign = Literal('@').suppress()
    equals = Literal('=').suppress()
    semi   = Literal(';').suppress()

    ident = Word(alphanums + '_.$')

    newline = Word('\r\n').suppress()

    dest = Word('AMD')
    unary = Combine(Optional(oneOf("- !")) + oneOf("0 1 A M D"))
    binary = Combine(oneOf("A M D") + oneOf("+ - & |") + oneOf("A M D 1"))
    comp = binary | unary
    jump = oneOf("JGT JEQ JGE JLT JNE JLE JMP")
    cinstruction = Group(Optional(dest + equals) + comp + Optional(semi + jump)).setParseAction(mkCInst)
    branchtarget = Group(lparen + ident + rparen).setParseAction(mkBranchTarget)
    aoperand = Word(nums).setParseAction(lambda toks: int(toks[0])) | ident
    ainstruction = Group(atsign + aoperand).setParseAction(mkAInst)
    command = ((cinstruction | ainstruction | branchtarget) + newline) | newline
    program = ZeroOrMore(command)
    program.ignore(cppStyleComment)

    return list(program.parseString(asm, parseAll=True))

def isa(a, type):
    return isinstance(a, type)

def nextinst(idx, ast):
    while idx < len(ast):
        if not isa(ast[idx], BranchTarget):
            return ast[idx].instnum

        idx += 1

    assert False, "no instruction after label"

def makeSymTab(ast):
    # populate with initial known values
    symtab = {
        'SP'     : 0,
        'LCL'    : 1,
        'ARG'    : 2,
        'THIS'   : 3,
        'THAT'   : 4,
        'SCREEN' : 16384,
        'KBD'    : 24576
    }

    # add R0-R15
    for i in xrange(16):
        symtab['R%d' % (i)] = i

    instnum = 0

    # first pass: number instructions
    for inst in ast:
        if isa(inst, BranchTarget):
            continue

        inst.instnum = instnum
        instnum += 1

    # second pass: get labels
    for (i, target) in enumerate(ast):
        if not isa(target, BranchTarget):
            continue

        if target.label not in symtab:
            symtab[target.label] = nextinst(i+1, ast)
        else:
            assert False, "branch label used twice"

    valuenum = 16 # start after registers

    for line in ast:
        if not isa(line, AInst):
            continue

        if isa(line.value, str):
            if not line.value in symtab:
                symtab[line.value] = valuenum
                valuenum += 1

    return symtab

def emitMachineCode(symtab, ast):
    insts = []
    for line in ast:
        if isa(line, BranchTarget):
            continue

        insts.append(line.emit(symtab))

    return '\n'.join(insts)

def assemble(asm):
    ast = parse(asm)
    symtab = makeSymTab(ast)
    output = emitMachineCode(symtab, ast)
    #return '\n'.join([str(x) for x in ast])
    return output

def main():
    parser = argparse.ArgumentParser(
        description='Assemble hack assembly to machine code.')
    parser.add_argument("inputasm", metavar='input.asm', help='input .asm file.')
    args = parser.parse_args()

    try:
        with open(args.inputasm, 'r') as f:
            asmstring = f.read()
    except IOError:
        print "Can't find file: %s" % (args.inputasm)
        sys.exit(1)

    output = assemble(asmstring)

    (root, _) = os.path.splitext(args.inputasm)
    outputfile = root + '.hack'
    try:
        with open(outputfile, 'w') as f:
            f.write(output)
    except IOError:
        print "Can't open file: %s, for writing" % (outputfile)
        sys.exit(1)

if __name__ == '__main__':
    main()

