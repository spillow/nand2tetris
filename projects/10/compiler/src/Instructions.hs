module Instructions(
    push, pop, add, sub, neg, eq, gt, lt, and, or,
    not, label, goto, ifGoto, function, call, return',
    local, constant, temp, pointer, this, that, argument, static,
    Instruction(Label), MemSeg) where

import Prelude hiding (and, or, not)

type Idx = Integer
type LabelName = String
type FuncName = String
type NumLocals = Integer
type NumArgs = Integer

data MemSeg = Local | Constant | Temp | Pointer | This | That | Arg | Static deriving (Eq)
data Instruction =
    Push MemSeg Idx
  | Pop MemSeg Idx
  | Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  | Label  LabelName
  | Goto   LabelName
  | IfGoto LabelName
  | Function FuncName NumLocals
  | Call FuncName NumArgs
  | Return

instance Show MemSeg where
    show Local    = "local"
    show Constant = "constant"
    show Temp     = "temp"
    show Pointer  = "pointer"
    show This     = "this"
    show That     = "that"
    show Arg      = "argument"
    show Static   = "static"

instance Show Instruction where
    show (Push m i) = "push " ++ show m ++ " " ++ show i
    show (Pop m i)  = "pop "  ++ show m ++ " " ++ show i
    show Add        = "add"
    show Sub        = "sub"
    show Neg        = "neg"
    show Eq         = "eq"
    show Gt         = "gt"
    show Lt         = "lt"
    show And        = "and"
    show Or         = "or"
    show Not        = "not"
    show (Label l)  = "label " ++ l
    show (Goto l)   = "goto " ++ l
    show (IfGoto l) = "if-goto " ++ l

    show (Function f n) = "function " ++ f ++ " " ++ show n
    show (Call f n)     = "call " ++ f ++ " " ++ show n
    show Return         = "return"

push :: MemSeg -> Idx -> Instruction
push = Push
pop :: MemSeg -> Idx -> Instruction
pop = Pop
add :: Instruction
add = Add
sub :: Instruction
sub = Sub
neg :: Instruction
neg = Neg
eq :: Instruction
eq = Eq
gt :: Instruction
gt = Gt
lt :: Instruction
lt = Lt
and :: Instruction
and = And
or :: Instruction
or = Or
not :: Instruction
not = Not
label :: LabelName -> Instruction
label = Label
goto :: LabelName -> Instruction
goto = Goto
ifGoto :: LabelName -> Instruction
ifGoto = IfGoto
function :: FuncName -> NumLocals -> Instruction
function = Function
call :: FuncName -> NumArgs -> Instruction
call = Call
return' :: Instruction
return' = Return

local :: MemSeg
local = Local
constant :: MemSeg
constant = Constant
temp :: MemSeg
temp = Temp
pointer :: MemSeg
pointer = Pointer
this :: MemSeg
this = This
that :: MemSeg
that = That
argument :: MemSeg
argument = Arg
static :: MemSeg
static = Static