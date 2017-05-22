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
    POINTER
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
};

MemorySegment getSegmentFromString(const std::string &Name)
{
    auto I = SegmentMap.find(Name);
    assert(I != SegmentMap.end() && "Couldn't find segment!");
    return I->second;
}

typedef std::string HackInst;
typedef std::vector<HackInst> HackSeq;

class Instruction
{
public:
    explicit Instruction(Opcode Op) : m_Opcode(Op) {}

    Opcode getOpcode() const { return m_Opcode; }
    virtual HackSeq emit() const = 0;
private:
    Opcode m_Opcode;
};

class ArithBool : public Instruction
{
public:
    explicit ArithBool(Opcode Op) :
        Instruction(Op) {}

    HackSeq emit() const override
    {
        switch (getOpcode())
        {
        case Opcode::ADD:
        case Opcode::SUB:
        case Opcode::NEG:
        case Opcode::EQ:
        case Opcode::GT:
        case Opcode::LT:
        case Opcode::AND:
        case Opcode::OR:
        case Opcode::NOT:
            break;
        default:
            assert(0 && "unknown arith/bool opcode!");
            break;
        }

        return HackSeq();
    }
private:
};

class MemAccess : public Instruction
{
public:
    explicit MemAccess(Opcode Op, MemorySegment Seg, unsigned Idx) :
        Instruction(Op), m_Seg(Seg), m_Idx(Idx) {}

    HackSeq emit() const override
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
    MemorySegment m_Seg;
    unsigned      m_Idx;

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

            };
        case MemorySegment::POINTER:
            return {

            };
        case MemorySegment::LOCAL:
        case MemorySegment::ARGUMENT:
        case MemorySegment::THIS:
        case MemorySegment::THAT:
            return {
                // addr = segmentPointer + i
                // *SP = *addr
                // SP++
            };
        }

        //assert(0);
        return HackSeq();
    }

    HackSeq emitPop() const
    {
        switch (m_Seg)
        {
            // No constant!
        case MemorySegment::STATIC:
            return {

            };
        case MemorySegment::POINTER:
            return {

            };
        case MemorySegment::LOCAL:
        case MemorySegment::ARGUMENT:
        case MemorySegment::THIS:
        case MemorySegment::THAT:
            // addr = segmentPointer + i
            // SP--
            // *addr = *SP
            return {

            };
        }

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

VMInstColl parse(const std::vector<std::string>& Lines)
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
            Insts.push_back(std::make_unique<ArithBool>(Op));
        }
        else if (isMemAccess(Op))
        {
            assert(Tokens.size() == 3 && "wrong number of args to push/pop!");
            auto Seg = getSegmentFromString(Tokens[1]);
            auto Idx = std::stoi(Tokens[2]);
            Insts.push_back(std::make_unique<MemAccess>(Op, Seg, Idx));
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

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        std::cout << "Usage: VMTranslator file.vm\n";
        return 1;
    }

    char *Filename = argv[1];

    auto Lines = getLines(Filename);
    auto Instructions = parse(Lines);

    auto HackInsts = translate(Instructions);

    return 0;
}