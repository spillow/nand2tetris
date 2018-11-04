module Instructions where

type Idx = Integer
data MemSeg = Local | Constant | Temp | Pointer | This | That | Arg | Static
data Instruction = Push MemSeg Idx | Pop MemSeg Idx
newtype CompileState = CompileState { labelIndex :: Integer } deriving (Show)

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

push = Push
pop = Pop

local = Local
constant = Constant
temp = Temp
pointer = Pointer
this = This
that = That
arg = Arg
static = Static