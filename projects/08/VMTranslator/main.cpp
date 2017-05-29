#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <regex>
#include <assert.h>
#include <unordered_map>
#include <memory>

enum class Opcode
{
    ADD,
    SUB,
    NEG,
    EQ,
    GT,
    LT,
    AND,
    OR,
    NOT,
    POP,
    PUSH,
    LABEL,
    GOTO,
    IF_GOTO,
    FUNCTION,
    CALL,
    RETURN
};

enum class MemorySegment
{
    STATIC,
    LOCAL,
    ARGUMENT,
    THIS,
    THAT,
    CONSTANT,
    POINTER,
    TEMP
};

bool isArithBool(Opcode Op)
{
    return Op >= Opcode::ADD && Op <= Opcode::NOT;
}

bool isMemAccess(Opcode Op)
{
    return Op == Opcode::POP || Op == Opcode::PUSH;
}

bool isProgFlow(Opcode Op)
{
    return Op >= Opcode::LABEL && Op <= Opcode::IF_GOTO;
}

static const std::unordered_map<std::string, Opcode> OpcodeMap =
{
    { "add",      Opcode::ADD      },
    { "sub",      Opcode::SUB      },
    { "neg",      Opcode::NEG      },
    { "eq",       Opcode::EQ       },
    { "gt",       Opcode::GT       },
    { "lt",       Opcode::LT       },
    { "and",      Opcode::AND      },
    { "or",       Opcode::OR       },
    { "not",      Opcode::NOT      },
    { "pop",      Opcode::POP      },
    { "push",     Opcode::PUSH     },
    { "label",    Opcode::LABEL    },
    { "goto",     Opcode::GOTO     },
    { "if-goto",  Opcode::IF_GOTO  },
    { "function", Opcode::FUNCTION },
    { "call",     Opcode::CALL     },
    { "return",   Opcode::RETURN   },
};

Opcode getOpcodeFromString(const std::string &Name)
{
    auto I = OpcodeMap.find(Name);
    assert(I != OpcodeMap.end() && "Couldn't find opcode!");
    return I->second;
}

static const std::unordered_map<std::string, MemorySegment> SegmentMap =
{
    { "static",   MemorySegment::STATIC   },
    { "local",    MemorySegment::LOCAL    },
    { "argument", MemorySegment::ARGUMENT },
    { "this",     MemorySegment::THIS     },
    { "that",     MemorySegment::THAT     },
    { "constant", MemorySegment::CONSTANT },
    { "pointer",  MemorySegment::POINTER  },
    { "temp",     MemorySegment::TEMP     },
};

MemorySegment getSegmentFromString(const std::string &Name)
{
    auto I = SegmentMap.find(Name);
    assert(I != SegmentMap.end() && "Couldn't find segment!");
    return I->second;
}

class Context
{
public:
    Context() : m_Num(0) {}
    std::string genSym()
    {
        auto Sym = "$sym." + std::to_string(m_Num);
        m_Num++;
        return Sym;
    }
private:
    unsigned m_Num;
};

typedef std::string HackInst;
typedef std::vector<HackInst> HackSeq;

class Hack
{
public:
    static HackInst getSegment(MemorySegment Seg)
    {
        switch (Seg)
        {
        case MemorySegment::LOCAL:
            return "@LCL";
        case MemorySegment::ARGUMENT:
            return "@ARG";
        case MemorySegment::THIS:
            return "@THIS";
        case MemorySegment::THAT:
            return "@THAT";
        default:
            break;
        }

        assert(0);
        return "";
    }
};

class Value
{
public:
    explicit Value(Context &C) : Ctx(C) {}
    virtual HackSeq emit() = 0;
    Context& getContext() { return Ctx; }
private:
    Context &Ctx;
};

class Function;

class Instruction : public Value
{
public:
    explicit Instruction(Context &C, Opcode Op, const Function *Func) :
        Value(C), m_Opcode(Op), m_Func(Func) {}

    Opcode getOpcode() const { return m_Opcode; }
    const Function* getParent() const { return m_Func; }

    std::string getFuncName() const;
    std::string getQualLabelName(const std::string &LabelName);
private:
    const Opcode m_Opcode;
    const Function *m_Func;
};

class Function : public Value
{
public:
    explicit Function(Context &C, std::string Name, unsigned NumLocalVars)
        : Value(C), m_Name(Name), m_NumLocalVars(NumLocalVars) {}
    void addInst(std::unique_ptr<Instruction> &&Inst)
    {
        m_Insts.push_back(std::move(Inst));
    }

    std::string getName() const { return m_Name; }

    HackSeq emit() override
    {
        // g:
        // repeat nVars times:
        // push 0
        HackSeq Insts {
            "// Begin Function: " + m_Name,
            "(" + m_Name + ")",
            "@LCL",
            "A=M",
        };
        for (unsigned i = 0; i < m_NumLocalVars; i++)
        {
            Insts.push_back("M=0");
            Insts.push_back("A=A+1");
        }

        // Update stack pointer
        Insts.push_back("D=A");
        Insts.push_back("@SP");
        Insts.push_back("M=D");

        // Now emit all of the instructions in the function
        for (auto &I : m_Insts)
        {
            auto Expansion = I->emit();
            Insts.insert(Insts.end(), Expansion.begin(), Expansion.end());
        }

        Insts.push_back("// End Function: " + m_Name);

        return Insts;
    }
private:
    std::vector<std::unique_ptr<Instruction>> m_Insts;
    const std::string m_Name;
    const unsigned m_NumLocalVars;
};

class Return : public Instruction
{
public:
    Return(Context &C, const Function *Func) : Instruction(C, Opcode::RETURN, Func) {}
    HackSeq emit() override
    {
        auto restoreReg = [](const std::string &Reg, unsigned Offset) -> HackSeq
        {
            return {
                "@R13",
                "D=M",
                "@" + std::to_string(Offset),
                "A=D-A",
                "D=M",
                "@" + Reg,
                "M=D",
            };
        };

        HackSeq Insts {
            "// return",
            // frame = LCL            // frame is a temp. variable
            "@LCL",
            "A=M",
            "D=A",
            "@R13", // frame
            "M=D",
            // retAddr = *(frame - 5) // retAddr is a temp. variable
            "@5",
            "A=D-A",
            "D=M",
            "@R14", // retAddr
            "M=D",
            // *ARG = pop             // repositions the return value
            "@SP",
            "M=M-1",
            "A=M",
            "D=M",
            "@ARG",
            "A=M",
            "M=D",
                                      // for the caller
            // SP = ARG + 1           // restores the caller�s SP
            "D=A+1",
            "@SP",
            "M=D",
        };
        // THAT = *(frame - 1)    // restores the caller�s THAT
        auto Seq = restoreReg("THAT", 1);
        Insts.insert(Insts.end(), Seq.begin(), Seq.end());
        // THIS = *(frame - 2)    // restores the caller�s THIS
        Seq = restoreReg("THIS", 2);
        Insts.insert(Insts.end(), Seq.begin(), Seq.end());
        // ARG = *(frame - 3)     // restores the caller�s ARG
        Seq = restoreReg("ARG", 3);
        Insts.insert(Insts.end(), Seq.begin(), Seq.end());
        //LCL = *(frame - 4)      // restores the caller�s LCL
        Seq = restoreReg("LCL", 4);
        Insts.insert(Insts.end(), Seq.begin(), Seq.end());

        // goto retAddr           // goto returnAddress
        Insts.push_back("@R14");
        Insts.push_back("A=M");
        Insts.push_back("0;JMP");

        return Insts;
    }
};

class Call : public Instruction
{
public:
    Call(Context &C, std::string Name, unsigned NumArgs, Function *Func) :
        Instruction(C, Opcode::CALL, Func), m_FuncName(Name), m_NumArgs(NumArgs) {}
    HackSeq emit() override
    {
        auto ReturnLabel = getQualLabelName("returnAddress") + getContext().genSym();
        return{
            "// call " + m_FuncName + " " + std::to_string(m_NumArgs),
            // push returnAddress   // saves the return address
            "@" + ReturnLabel,
            "D=A",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1",
            // push LCL             // saves the LCL of f
            "@LCL",
            "A=M",
            "D=A",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1",
            // push ARG             // saves the ARG of f
            "@ARG",
            "A=M",
            "D=A",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1",
            // push THIS            // saves the THIS of f
            "@THIS",
            "A=M",
            "D=A",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1",
            // push THAT            // saves the THAT of f
            "@THAT",
            "A=M",
            "D=A",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1",
            // ARG = SP - nArgs - 5 // repositions SP for g
            "@SP",
            "D=M",
            "@" + std::to_string(m_NumArgs),
            "D=D-A",
            "@5",
            "D=D-A",
            "@ARG",
            "M=D",
            // LCL = SP             // repositions LCL for g
            "@SP",
            "D=M",
            "@LCL",
            "M=D",
            // goto g               // transfers control to g
            "@" + m_FuncName,
            "0;JMP",
            // returnAddress :      // the generated symbol
            "(" + ReturnLabel + ")"
        };
    }
private:
    const std::string m_FuncName;
    const unsigned m_NumArgs;
};

std::string Instruction::getQualLabelName(const std::string &LabelName)
{
    return getFuncName() + "$" + LabelName;
}

std::string Instruction::getFuncName() const
{
    return getParent() ? getParent()->getName() : "";
}

class ProgFlow : public Instruction
{
public:
    explicit ProgFlow(Context &C, Opcode Op, std::string LabelName, const Function *Func) :
        Instruction(C, Op, Func), m_LabelName(LabelName) {}

    HackSeq emit() override
    {
        switch (getOpcode())
        {
        case Opcode::LABEL:
            return { "(" + getQualLabelName(m_LabelName) + ")" };
        case Opcode::GOTO:
            return {
                "@" + getQualLabelName(m_LabelName),
                "0;JMP"
            };
        case Opcode::IF_GOTO:
            return {
                // SP--
                "@SP",
                "M=M-1",
                // x = *SP
                "A=M",
                "D=M",
                // if (x) goto label
                "@" + getQualLabelName(m_LabelName),
                "D;JNE"
            };
        default:
            break;
        }
        assert(0);
        return HackSeq();
    }
private:
    const std::string m_LabelName;
};

class ArithBool : public Instruction
{
public:
    explicit ArithBool(Context &C, Opcode Op, const Function *Func) :
        Instruction(C, Op, Func) {}

    HackSeq emit() override
    {
        switch (getOpcode())
        {
        case Opcode::ADD:
        case Opcode::SUB:
        case Opcode::AND:
        case Opcode::OR:
            return {
                // SP--
                "@SP",
                "M=M-1",
                // y = *SP
                "A=M",
                "D=M",
                // SP--
                "@SP",
                "M=M-1",
                // *SP = *SP op y
                "A=M",
                operation(),
                // SP++
                "@SP",
                "M=M+1"
            };
        case Opcode::NEG:
        case Opcode::NOT:
        {
            std::string Op = (getOpcode() == Opcode::NEG) ? "-" : "!";
            return{
                // SP--
                "@SP",
                "M=M-1",
                // *SP = op *SP
                "A=M",
                "M=" + Op + "M",
                // SP++
                "@SP",
                "M=M+1"
            };
        }
        case Opcode::EQ:
        case Opcode::GT:
        case Opcode::LT:
        {
            auto TrueSym = getContext().genSym();
            auto JoinSym = getContext().genSym();
            return {
                // SP--
                "@SP",
                "M=M-1",
                // y = *SP
                "A=M",
                "D=M",
                // SP--
                "@SP",
                "M=M-1",
                // *SP = *SP - y
                "A=M",
                "D=M-D",
                // *SP;comp (TRUE)
                "@TRUE" + TrueSym,
                "D;" + comparison(),
                // *SP = 0
                "@SP",
                "A=M",
                "M=0",
                // goto JOIN
                "@JOIN" + JoinSym,
                "0;JMP",
                // (TRUE)
                "(TRUE" + TrueSym + ")",
                // *SP = -1
                "@SP",
                "A=M",
                "M=-1",
                // (JOIN)
                "(JOIN" + JoinSym + ")",
                // SP++
                "@SP",
                "M=M+1"
            };
        }
        default:
            assert(0 && "unknown arith/bool opcode!");
            break;
        }

        assert(0);
        return HackSeq();
    }
private:
    std::string comparison() const
    {
        switch (getOpcode())
        {
        case Opcode::EQ:
            return "JEQ";
        case Opcode::GT:
            return "JGT";
        case Opcode::LT:
            return "JLT";
        default:
            break;
        }

        assert(0);
        return "";
    }

    std::string operation() const
    {
        switch (getOpcode())
        {
        case Opcode::ADD:
            return "M=D+M";
        case Opcode::SUB:
            return "M=M-D";
        case Opcode::AND:
            return "M=D&M";
        case Opcode::OR:
            return "M=D|M";
        }

        assert(0);
        return "";
    }
};

class MemAccess : public Instruction
{
public:
    explicit MemAccess(Context &C, Opcode Op, MemorySegment Seg, unsigned Idx,
        const std::string &Filename, const Function *Func) :
        Instruction(C, Op, Func), m_Seg(Seg), m_Idx(Idx), m_Filename(Filename) {}

    HackSeq emit() override
    {
        switch (getOpcode())
        {
        case Opcode::POP:
            return emitPop(m_Seg, m_Idx, m_Filename);
        case Opcode::PUSH:
            return emitPush(m_Seg, m_Idx, m_Filename);
        default:
            assert(0 && "unknown push/pop opcode!");
            break;
        }
        return HackSeq();
    }

    static HackSeq emitPush(MemorySegment Seg, unsigned Idx, const std::string Filename = "")
    {
        switch (Seg)
        {
        case MemorySegment::CONSTANT:
            return{
                // *SP = i
                "@" + std::to_string(Idx),
                "D=A",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::STATIC:
            return{
                // addr = <filename>.idx
                "@" + Filename + "." + std::to_string(Idx),
                // *SP = *addr
                "D=M",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::POINTER:
            // accessing pointer 0 should result in accessing THIS
            // accessing pointer 1 should result in accessing THAT
            assert(Idx == 0 || Idx == 1);
            return{
                // *SP = THIS/THAT
                "@" + getPointer(Idx),
                "D=M",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::TEMP:
            return{
                // addr = 5 + i
                "@" + std::to_string(Idx),
                "D=A",
                "@5",
                "A=D+A",
                // *SP = *addr
                "D=M",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::LOCAL:
        case MemorySegment::ARGUMENT:
        case MemorySegment::THIS:
        case MemorySegment::THAT:
            return{
                // addr = segmentPointer + i
                "@" + std::to_string(Idx),
                "D=A",
                Hack::getSegment(Seg),
                "A=M",
                "A=D+A",
                // *SP = *addr
                "D=M",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        }

        assert(0);
        return HackSeq();
    }

    static HackSeq emitPop(MemorySegment Seg, unsigned Idx, const std::string Filename = "")
    {
        switch (Seg)
        {
            // No constant!
        case MemorySegment::STATIC:
            return{
                // SP--
                "@SP",
                "M=M-1",
                // *SP
                "A=M",
                "D=M",
                // addr = <filename>.idx
                "@" + Filename + "." + std::to_string(Idx),
                // *addr = *SP
                "M=D"
            };
        case MemorySegment::POINTER:
            return{
                // SP--
                "@SP",
                "M=M-1",
                "A=M",
                "D=M",
                // THIS/THAT = *SP
                "@" + getPointer(Idx),
                "M=D"
            };
        case MemorySegment::TEMP:
            return{
                // addr = 5 + i
                "@" + std::to_string(Idx),
                "D=A",
                "@5",
                "D=D+A",
                "@R13",
                "M=D",
                // SP--
                "@SP",
                "M=M-1",
                // *addr = *SP
                "A=M",
                "D=M",
                "@R13",
                "A=M",
                "M=D"
            };
        case MemorySegment::LOCAL:
        case MemorySegment::ARGUMENT:
        case MemorySegment::THIS:
        case MemorySegment::THAT:
            return{
                // addr = segmentPointer + i
                "@" + std::to_string(Idx),
                "D=A",
                Hack::getSegment(Seg),
                "A=M",
                "D=D+A",
                "@R13",
                "M=D",
                // SP--
                "@SP",
                "M=M-1",
                // *addr = *SP
                "A=M",
                "D=M",
                "@R13",
                "A=M",
                "M=D"
            };
        }

        assert(0);
        return HackSeq();
    }

private:
    const MemorySegment m_Seg;
    const unsigned      m_Idx;
    const std::string   &m_Filename;

    static std::string getPointer(unsigned Idx)
    {
        return (Idx == 0) ? "THIS" : "THAT";
    }
};

std::vector<std::string> getLines(const std::string &Filename)
{
    std::ifstream VmFile(Filename);
    std::string Line;
    std::vector<std::string> Lines;
    while (std::getline(VmFile, Line))
        Lines.push_back(Line);

    return Lines;
}

std::vector<std::string> tokens(const std::string& Line)
{
    auto Pos = Line.find("//");
    std::string S = (Pos == std::string::npos) ?
        Line :
        Line.substr(0, Pos);

    std::regex NonSpace(R"(\S+)");

    auto WordsBegin = std::sregex_iterator(S.begin(), S.end(), NonSpace);
    auto WordsEnd = std::sregex_iterator();

    std::vector<std::string> Words;
    for (auto I = WordsBegin; I != WordsEnd; I++)
        Words.push_back(I->str());

    return Words;
}

typedef std::vector<std::unique_ptr<Value>> VMValueSeq;

VMValueSeq parse(Context &C, const std::vector<std::string>& Lines, const std::string &Filename)
{
    VMValueSeq Values;
    std::unique_ptr<Function> CurrFunc;

    auto addInst = [&](auto &&V)
    {
        if (CurrFunc)
            CurrFunc->addInst(std::move(V));
        else
            Values.push_back(std::move(V));
    };

    for (auto &L : Lines)
    {
        auto Tokens = tokens(L);
        
        if (Tokens.empty())
            continue;

        auto Op = getOpcodeFromString(Tokens[0]);

        if (isArithBool(Op))
        {
            addInst(std::make_unique<ArithBool>(C, Op, CurrFunc.get()));
        }
        else if (isMemAccess(Op))
        {
            assert(Tokens.size() == 3 && "wrong number of args to push/pop!");
            auto Seg = getSegmentFromString(Tokens[1]);
            auto Idx = std::stoi(Tokens[2]);
            addInst(std::make_unique<MemAccess>(C, Op, Seg, Idx, Filename, CurrFunc.get()));
        }
        else if (isProgFlow(Op))
        {
            assert(Tokens.size() == 2 && "wrong number of args to flow inst!");
            std::string LabelName = Tokens[1];
            addInst(std::make_unique<ProgFlow>(C, Op, LabelName, CurrFunc.get()));
        }
        else if (Op == Opcode::FUNCTION)
        {
            if (CurrFunc)
                Values.push_back(std::move(CurrFunc));

            assert(Tokens.size() == 3 && "wrong number of args to function inst!");

            std::string FuncName = Tokens[1];
            unsigned NumLocalVars = std::stoi(Tokens[2]);
            CurrFunc.reset(new Function(C, FuncName, NumLocalVars));
        }
        else if (Op == Opcode::CALL)
        {
            assert(Tokens.size() == 3 && "wrong number of args to call inst!");

            std::string FuncName = Tokens[1];
            unsigned NumArgs = std::stoi(Tokens[2]);
            addInst(std::make_unique<Call>(C, FuncName, NumArgs, CurrFunc.get()));
        }
        else if (Op == Opcode::RETURN)
        {
            addInst(std::make_unique<Return>(C, CurrFunc.get()));
        }
        else
        {
            assert(0 && "unknown opcode!");
        }
    }

    if (CurrFunc)
        Values.push_back(std::move(CurrFunc));

    return Values;
}

HackSeq translate(const VMValueSeq &Values)
{
    HackSeq HackInsts;
    for (auto &V : Values)
    {
        auto Trans = V->emit();
        HackInsts.insert(std::end(HackInsts), std::begin(Trans), std::end(Trans));
    }

    return HackInsts;
}

std::string getFilename(const std::string &Path)
{
    auto Name = Path.substr(Path.find_last_of(R"(\/)") + 1);
    return Name.substr(0, Name.find_last_of("."));
}

void outputToFile(const std::string &Output, const HackSeq &Insts)
{
    std::ofstream Out(Output);

    for (auto &I : Insts)
    {
        Out << I << "\n";
    }
}

HackSeq emitBootstrap(Context &Ctx)
{
    const unsigned STACK_START_ADDR = 256;
    HackSeq SPInit = {
        "@" + std::to_string(STACK_START_ADDR),
        "D=A",
        "@SP",
        "M=D"
    };

    Call call(Ctx, "Sys.init", 0, nullptr);
    auto CallCode = call.emit();

    SPInit.insert(std::end(SPInit), std::begin(CallCode), std::end(CallCode));
    return SPInit;
}

int main(int argc, char **argv)
{
    if (argc < 3)
    {
        std::cout << "Usage: VMTranslator <file.vm>+ <output.asm>\n";
        return 1;
    }

    Context Ctx;

    HackSeq LinkedHackInsts = emitBootstrap(Ctx);

    for (unsigned i = 1; i < argc - 1; i++)
    {
        const char *Filepath = argv[i];

        auto Lines = getLines(Filepath);
        auto Filename = getFilename(Filepath);
        auto Instructions = parse(Ctx, Lines, Filename);

        auto HackInsts = translate(Instructions);
        LinkedHackInsts.insert(
            std::end(LinkedHackInsts), std::begin(HackInsts), std::end(HackInsts));
    }

    const char *Output = argv[argc - 1];

    outputToFile(Output, LinkedHackInsts);

    return 0;
}