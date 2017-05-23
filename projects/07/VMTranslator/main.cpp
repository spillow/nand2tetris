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
    PUSH
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
    return Op < Opcode::POP;
}

bool isMemAccess(Opcode Op)
{
    return Op == Opcode::POP || Op == Opcode::PUSH;
}

static const std::unordered_map<std::string, Opcode> OpcodeMap =
{
    { "add",  Opcode::ADD  },
    { "sub",  Opcode::SUB  },
    { "neg",  Opcode::NEG  },
    { "eq",   Opcode::EQ   },
    { "gt",   Opcode::GT   },
    { "lt",   Opcode::LT   },
    { "and",  Opcode::AND  },
    { "or",   Opcode::OR   },
    { "not",  Opcode::NOT  },
    { "pop",  Opcode::POP  },
    { "push", Opcode::PUSH },
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

class Instruction
{
public:
    explicit Instruction(Context &C, Opcode Op) : m_Opcode(Op), Ctx(C) {}

    Opcode getOpcode() const { return m_Opcode; }
    Context& getContext() { return Ctx; }
    virtual HackSeq emit() = 0;
private:
    const Opcode m_Opcode;
    Context &Ctx;
};

class ArithBool : public Instruction
{
public:
    explicit ArithBool(Context &C, Opcode Op) :
        Instruction(C, Op) {}

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
        const std::string &Filename) :
        Instruction(C, Op), m_Seg(Seg), m_Idx(Idx), m_Filename(Filename) {}

    HackSeq emit() override
    {
        switch (getOpcode())
        {
        case Opcode::POP:
            return emitPop();
        case Opcode::PUSH:
            return emitPush();
        default:
            assert(0 && "unknown push/pop opcode!");
            break;
        }
        return HackSeq();
    }

private:
    const MemorySegment m_Seg;
    const unsigned      m_Idx;
    const std::string   &m_Filename;

    std::string getPointer() const
    {
        return (m_Idx == 0) ? "THIS" : "THAT";
    }

    HackSeq emitPush() const
    {
        switch (m_Seg)
        {
        case MemorySegment::CONSTANT:
            return {
                // *SP = i
                "@" + std::to_string(m_Idx),
                "D=A",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::STATIC:
            return {
                // addr = <filename>.idx
                "@" + m_Filename + "." + std::to_string(m_Idx),
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
            assert(m_Idx == 0 || m_Idx == 1);
            return {
                // *SP = THIS/THAT
                "@" + getPointer(),
                "D=M",
                "@SP",
                "A=M",
                "M=D",
                // SP++
                "@SP",
                "M=M+1"
            };
        case MemorySegment::TEMP:
            return {
                // addr = 5 + i
                "@" + std::to_string(m_Idx),
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
            return {
                // addr = segmentPointer + i
                "@" + std::to_string(m_Idx),
                "D=A",
                Hack::getSegment(m_Seg),
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

    HackSeq emitPop() const
    {
        switch (m_Seg)
        {
            // No constant!
        case MemorySegment::STATIC:
            return {
                // SP--
                "@SP",
                "M=M-1",
                // *SP
                "A=M",
                "D=M",
                // addr = <filename>.idx
                "@" + m_Filename + "." + std::to_string(m_Idx),
                // *addr = *SP
                "M=D"
            };
        case MemorySegment::POINTER:
            return {
                // SP--
                "@SP",
                "M=M-1",
                "A=M",
                "D=M",
                // THIS/THAT = *SP
                "@" + getPointer(),
                "M=D"
            };
        case MemorySegment::TEMP:
            return {
                // addr = 5 + i
                "@" + std::to_string(m_Idx),
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
            return {
                // addr = segmentPointer + i
                "@" + std::to_string(m_Idx),
                "D=A",
                Hack::getSegment(m_Seg),
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

std::vector<std::string> tokens(const std::string& S)
{
    std::regex Comment(R"(^\s*//)");

    if (std::regex_search(S, Comment))
        return std::vector<std::string>();

    std::regex NonSpace(R"(\S+)");

    auto WordsBegin = std::sregex_iterator(S.begin(), S.end(), NonSpace);
    auto WordsEnd = std::sregex_iterator();

    std::vector<std::string> Words;
    for (auto I = WordsBegin; I != WordsEnd; I++)
        Words.push_back(I->str());

    return Words;
}

typedef std::vector<std::unique_ptr<Instruction>> VMInstColl;

VMInstColl parse(Context &C, const std::vector<std::string>& Lines, const std::string &Filename)
{
    VMInstColl Insts;
    for (auto &L : Lines)
    {
        auto Tokens = tokens(L);
        
        if (Tokens.empty())
            continue;

        auto Op = getOpcodeFromString(Tokens[0]);

        if (isArithBool(Op))
        {
            Insts.push_back(std::make_unique<ArithBool>(C, Op));
        }
        else if (isMemAccess(Op))
        {
            assert(Tokens.size() == 3 && "wrong number of args to push/pop!");
            auto Seg = getSegmentFromString(Tokens[1]);
            auto Idx = std::stoi(Tokens[2]);
            Insts.push_back(std::make_unique<MemAccess>(C, Op, Seg, Idx, Filename));
        }
    }

    return Insts;
}

HackSeq translate(const VMInstColl &Insts)
{
    HackSeq HackInsts;
    for (auto &I : Insts)
    {
        auto Trans = I->emit();
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

int main(int argc, char **argv)
{
    if (argc != 3)
    {
        std::cout << "Usage: VMTranslator <file.vm> <output.asm>\n";
        return 1;
    }

    const char *Filepath = argv[1];
    const char *Output   = argv[2];

    Context Ctx;

    auto Lines = getLines(Filepath);
    auto Filename = getFilename(Filepath);
    auto Instructions = parse(Ctx, Lines, Filename);

    auto HackInsts = translate(Instructions);

    outputToFile(Output, HackInsts);

    return 0;
}