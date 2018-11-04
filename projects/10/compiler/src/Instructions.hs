module Instructions where

type Idx = Integer
type LabelName = String
type FuncName = String
type NumLocals = Integer
type NumArgs = Integer

data MemSeg = Local | Constant | Temp | Pointer | This | That | Arg | Static
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

push = Push
pop = Pop
add = Add
sub = Sub
neg = Neg
eq = Eq
gt = Gt
lt = Lt
and = And
or = Or
not = Not
label = Label
goto = Goto
ifGoto = IfGoto
function = Function
call = Call
return' = Return

local = Local
constant = Constant
temp = Temp
pointer = Pointer
this = This
that = That
arg = Arg
static = Static