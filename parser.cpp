/*
#include<bits/stdc++.h>
*/
#include <algorithm>
#include <cstdint>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <ostream>
#include <string>
#include <tuple>
#include <variant>

#pragma GCC optimize("O3")

using namespace std;

using Opcode = uint8_t;
constexpr Opcode OPC_3 = 0b0000011;
constexpr Opcode OPC_15 = 0b0001111;
constexpr Opcode OPC_19 = 0b0010011;
constexpr Opcode OPC_23 = 0b0010111;
constexpr Opcode OPC_35 = 0b0100011;
constexpr Opcode OPC_51 = 0b0110011;
constexpr Opcode OPC_55 = 0b0110111;
constexpr Opcode OPC_99 = 0b1100011;
constexpr Opcode OPC_103 = 0b1100111;
constexpr Opcode OPC_111 = 0b1101111;
constexpr Opcode OPC_115 = 0b1110011;

using Funct3 = uint8_t;
constexpr Funct3 F0 = 0b000;
constexpr Funct3 F1 = 0b001;
constexpr Funct3 F2 = 0b010;
constexpr Funct3 F3 = 0b011;
constexpr Funct3 F4 = 0b100;
constexpr Funct3 F5 = 0b101;
constexpr Funct3 F6 = 0b110;
constexpr Funct3 F7 = 0b111;

using Funct7 = uint8_t;
constexpr Funct7 F0_7 = 0b0000000;
constexpr Funct7 F1_7 = 0b0000001;
constexpr Funct7 F32_7 = 0b0100000;

string currentPolicy = "LRU";

struct R_Type {
    uint8_t rd;
    Funct3 funct3;
    uint8_t rs1;
    uint8_t rs2;
    Funct7 funct7;
};

struct I_Type {
    uint8_t rd;
    Funct3 funct3;
    uint8_t rs1;
    int32_t imm;
};

struct S_Type {
    Funct3 funct3;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
};

struct B_Type {
    Funct3 funct3;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
};

struct U_Type {
    uint8_t rd;
    int32_t imm;
};

struct J_Type {
    uint8_t rd;
    int32_t imm;
};

struct Fence_Type {
    uint8_t pred;
    uint8_t succ;
};

struct System_Type {
    string sys_call;

    explicit System_Type(string str) { sys_call = str; }
};


struct Instruction {
    string name;
    string type_name;
    Opcode opcode;
    variant<R_Type, I_Type, S_Type, B_Type, U_Type, J_Type, Fence_Type, System_Type> type;

    Instruction(string name, Opcode opcode, R_Type r) {
        this->name = name, this->opcode = opcode, this->type = r, this->type_name = "R_type";
    }

    Instruction(string name, Opcode opcode, I_Type i) {
        this->name = name, this->opcode = opcode, this->type = i, this->type_name = "I_type";
    }

    Instruction(string name, Opcode opcode, S_Type s) {
        this->name = name, this->opcode = opcode, this->type = s, this->type_name = "S_type";
    }

    Instruction(string name, Opcode opcode, B_Type b) {
        this->name = name, this->opcode = opcode, this->type = b, this->type_name = "B_type";
    }

    Instruction(string name, Opcode opcode, U_Type u) {
        this->name = name, this->opcode = opcode, this->type = u, this->type_name = "U_type";
    }

    Instruction(string name, Opcode opcode, J_Type j) {
        this->name = name, this->opcode = opcode, this->type = j, this->type_name = "J_type";
    }

    Instruction(string name, Opcode opcode, Fence_Type fence) {
        this->name = name, this->opcode = opcode, this->type = fence, this->type_name = "Fence_type";
    }

    Instruction(string name, Opcode opcode, System_Type system) {
        this->name = name, this->opcode = opcode, this->type = system, this->type_name = "System_type";
    }
};


map<string, int> registers = {{"zero", 0}, {"ra", 1},   {"sp", 2},   {"gp", 3},   {"tp", 4},   {"t0", 5},   {"t1", 6},
                              {"t2", 7},   {"s0", 8},   {"fp", 8},   {"s1", 9},   {"a0", 10},  {"a1", 11},  {"a2", 12},
                              {"a3", 13},  {"a4", 14},  {"a5", 15},  {"a6", 16},  {"a7", 17},  {"s2", 18},  {"s3", 19},
                              {"s4", 20},  {"s5", 21},  {"s6", 22},  {"s7", 23},  {"s8", 24},  {"s9", 25},  {"s10", 26},
                              {"s11", 27}, {"t3", 28},  {"t4", 29},  {"t5", 30},  {"t6", 31},


                              {"r0", 0},   {"r1", 1},   {"r2", 2},   {"r3", 3},   {"r4", 4},   {"r5", 5},   {"r6", 6},
                              {"r7", 7},   {"r8", 8},   {"r9", 8},   {"r10", 9},  {"r11", 10}, {"r11", 11}, {"r12", 12},
                              {"r13", 13}, {"r14", 14}, {"r15", 15}, {"r16", 16}, {"r17", 17}, {"r18", 18}, {"r19", 19},
                              {"r20", 20}, {"r21", 21}, {"r22", 22}, {"r23", 23}, {"r24", 24}, {"r25", 25}, {"r26", 26},
                              {"r27", 27}, {"r28", 28}, {"r29", 29}, {"r30", 30}, {"r31", 31}};

string trim(string s) {
    // убирает пробелы в начале и в конце слова
    size_t begin = 0;
    while (begin < s.size() && isspace(s[begin])) {
        begin++;
    }
    int end = s.size() - 1;
    while (end > begin && isspace(s[end])) {
        end--;
    }
    return s.substr(begin, end - begin + 1);
}

struct Parser {
    string filename;
    deque<Instruction> instructions;

    explicit Parser(string filename) { this->filename = filename; }

    static int get_register(string reg) {
        if (reg[0] == 'x') {
            return stoi(reg.substr(1));
        }
        return registers[reg];
    }

    static int parse_imm(string immStr) {
        string s = immStr;
        bool sign = false;
        if (!s.empty() && s[0] == '-') {
            sign = true;
            s = s.substr(1);
        }
        int ans = (s.size() > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) ? stoi(s.substr(2), nullptr, 16)
                                                                                : stoi(s);
        return ((sign) ? -ans : ans);
    }

    static uint8_t makeArg(string str) {
        bool isNum = ((str.size() > 0) ? true : false);
        for (size_t i = 0; i < str.size(); i++) {
            if (str[i] > '9' || str[i] < '0') {
                isNum = false;
            }
        }
        if (isNum) {
            return static_cast<uint8_t>(stoi(str));
        } else {
            uint8_t ans = 0;
            for (char chr: str) {
                if (chr == 'w') {
                    ans |= 1;
                }
                if (chr == 'r') {
                    ans |= 2;
                }
                if (chr == 'o') {
                    ans |= 4;
                }
                if (chr == 'i') {
                    ans |= 8;
                }
            }
            return ans;
        }
    }

    static Instruction makeInstruction(string command, deque<string> details) {
        if (command == "add" || command == "sub" || command == "sll" || command == "slt" || command == "sltu" ||
            command == "xor" || command == "srl" || command == "sra" || command == "or" || command == "and" ||
            command == "mul" || command == "mulh" || command == "mulhsu" || command == "mulhu" || command == "div" ||
            command == "divu" || command == "rem" || command == "remu") {
            return makeOP(command, details);
        }
        if (command == "addi" || command == "slti" || command == "sltiu" || command == "xori" || command == "ori" ||
            command == "andi" || command == "slli" || command == "srli" || command == "srai") {
            return makeOP_IMM(command, details);
        }
        if (command == "beq" || command == "bne" || command == "blt" || command == "bge" || command == "bltu" ||
            command == "bgeu") {
            return makeBRANCH(command, details);
        }
        if (command == "lb" || command == "lh" || command == "lw" || command == "lbu" || command == "lhu") {
            return makeLOAD(command, details);
        }
        if (command == "jal") {
            return makeJAL(command, details);
        }
        if (command == "jalr") {
            return makeJALR(command, details);
        }
        if (command == "sb" || command == "sh" || command == "sw") {
            return makeSTORE(command, details);
        }
        if (command == "lui") {
            return makeLUI(command, details);
        }
        if (command == "auipc") {
            return makeAUIPC(command, details);
        }
        if (command == "fence") {
            return makeFENCE(command, details);
        }
        if (command == "ecall" || command == "ebreak" || command == "pause" || command == "fence.tso" ||
            command == "nop") {
            return makeSYSTEM(command);
        }
    }


    static Instruction makeOP(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[1]));
        uint8_t rs2 = static_cast<uint8_t>(get_register(details[2]));
        Funct3 f3;
        Funct7 f7;

        if (command == "add") {
            f3 = F0;
            f7 = F0_7;
        }
        if (command == "sub") {
            f3 = F0;
            f7 = F32_7;
        }
        if (command == "sll") {
            f3 = F1;
            f7 = F0_7;
        }
        if (command == "slt") {
            f3 = F2;
            f7 = F0_7;
        }
        if (command == "sltu") {
            f3 = F3;
            f7 = F0_7;
        }
        if (command == "xor") {
            f3 = F4;
            f7 = F0_7;
        }
        if (command == "srl") {
            f3 = F5;
            f7 = F0_7;
        }
        if (command == "sra") {
            f3 = F5;
            f7 = F32_7;
        }
        if (command == "or") {
            f3 = F6;
            f7 = F0_7;
        }
        if (command == "and") {
            f3 = F7;
            f7 = F0_7;
        }
        if (command == "mul") {
            f3 = F0;
            f7 = F1_7;
        }
        if (command == "mulh") {
            f3 = F1;
            f7 = F1_7;
        }
        if (command == "mulhsu") {
            f3 = F2;
            f7 = F1_7;
        }
        if (command == "mulhu") {
            f3 = F3;
            f7 = F1_7;
        }
        if (command == "div") {
            f3 = F4;
            f7 = F1_7;
        }
        if (command == "divu") {
            f3 = F5;
            f7 = F1_7;
        }
        if (command == "rem") {
            f3 = F6;
            f7 = F1_7;
        }
        if (command == "remu") {
            f3 = F7;
            f7 = F1_7;
        }
        R_Type r{rd, f3, rs1, rs2, f7};
        return Instruction(command, OPC_51, r);
    }

    static Instruction makeOP_IMM(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[1]));
        int32_t imm = parse_imm(details[2]);
        Funct3 f3;

        if (command == "addi") {
            f3 = F0;
        }
        if (command == "slti") {
            f3 = F2;
        }
        if (command == "sltiu") {
            f3 = F3;
        }
        if (command == "xori") {
            f3 = F4;
        }
        if (command == "ori") {
            f3 = F6;
        }
        if (command == "andi") {
            f3 = F7;
        }
        if (command == "slli") {
            f3 = F1;
        }
        if (command == "srli") {
            f3 = F5;
        }
        if (command == "srai") {
            f3 = F5;
            imm += 1024;
        }
        I_Type i = {rd, f3, rs1, imm};
        return Instruction(command, OPC_19, i);
    }

    static Instruction makeBRANCH(string command, deque<string> details) {
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[0]));
        uint8_t rs2 = static_cast<uint8_t>(get_register(details[1]));
        int32_t imm = parse_imm(details[2]);
        Funct3 f3;

        if (command == "beq") {
            f3 = F0;
        }
        if (command == "bne") {
            f3 = F1;
        }
        if (command == "blt") {
            f3 = F4;
        }
        if (command == "bge") {
            f3 = F5;
        }
        if (command == "bltu") {
            f3 = F6;
        }
        if (command == "bgeu") {
            f3 = F7;
        }
        B_Type b = {f3, rs1, rs2, imm};
        return Instruction(command, OPC_99, b);
    }

    static Instruction makeLOAD(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        int32_t imm = parse_imm(details[1]);
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[2]));
        Funct3 f3;

        if (command == "lb") {
            f3 = F0;
        }
        if (command == "lh") {
            f3 = F1;
        }
        if (command == "lw") {
            f3 = F2;
        }
        if (command == "lbu") {
            f3 = F4;
        }
        if (command == "lhu") {
            f3 = F5;
        }
        I_Type i = {rd, f3, rs1, imm};
        return Instruction(command, OPC_3, i);
    }

    static Instruction makeSTORE(string command, deque<string> details) {
        uint8_t rs2 = static_cast<uint8_t>(get_register(details[0]));
        int32_t imm = parse_imm(details[1]);
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[2]));
        Funct3 f3;

        if (command == "sb") {
            f3 = F0;
        }
        if (command == "sh") {
            f3 = F1;
        }
        if (command == "sw") {
            f3 = F2;
        }
        S_Type s{f3, rs1, rs2, imm};
        return Instruction(command, OPC_35, s);
    }

    static Instruction makeLUI(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        int32_t imm = parse_imm(details[1]);
        U_Type u = {rd, imm};
        return Instruction(command, OPC_55, u);
    }

    static Instruction makeAUIPC(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        int32_t imm = parse_imm(details[1]);
        U_Type u = {rd, imm};
        return Instruction(command, OPC_23, u);
    }

    static Instruction makeJAL(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        int32_t imm = parse_imm(details[1]);
        J_Type j = {rd, imm};
        return Instruction(command, OPC_111, j);
    }

    static Instruction makeJALR(string command, deque<string> details) {
        uint8_t rd = static_cast<uint8_t>(get_register(details[0]));
        uint8_t rs1 = static_cast<uint8_t>(get_register(details[1]));
        int32_t imm = parse_imm(details[2]);
        I_Type it = {rd, F0, rs1, imm};
        return Instruction(command, OPC_103, it);
    }

    static Instruction makeFENCE(string command, deque<string> details) {
        uint8_t pred_val = makeArg(details[0]);
        uint8_t succ_val = makeArg(details[1]);
        Fence_Type ft{pred_val, succ_val};
        return Instruction(command, OPC_15, ft);
    }

    static Instruction makeSYSTEM(string command) {
        System_Type sys_type(command);
        return Instruction(command, OPC_15, sys_type);
    }

    deque<Instruction> parse() {
        ifstream in(filename);
        string str;
        deque<Instruction> instructions;
        while (getline(in, str)) {
            if (str.size() == 0) {
                continue;
            }
            deque<string> arguments;
            string command;
            for (char c: str) {
                if (c == ' ' || c == ',') {
                    if (!command.empty()) {
                        arguments.push_back(trim(command));
                        command.clear();
                    }
                } else {
                    command.push_back(c);
                }
            }
            if (!command.empty()) {
                arguments.push_back(trim(command));
            }
            command = arguments[0];
            arguments.pop_front();
            instructions.push_back(makeInstruction(command, arguments));
        }
        return instructions;
    }
};


struct CPU {
    uint32_t progCount = 0;
    deque<uint32_t> registers;


    explicit CPU() {
        registers.resize(32);
        fill(registers.begin(), registers.end(), 0);
    }


    void runCommand(const Instruction &instr) {

        Opcode opcode = instr.opcode;

        auto type = instr.type;

        switch (opcode) {
            case OPC_3: {
                I_Type i = get<I_Type>(type);
                if (i.rd != 0) {
                    registers[i.rd] = 0;
                }
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                break;
            }

            case OPC_19: {
                I_Type i = get<I_Type>(type);
                int32_t rs1_value = static_cast<int32_t>(registers[i.rs1]);

                switch (i.funct3) {
                    case F0: // ADDI
                    {
                        if (i.rd != 0) {
                            registers[i.rd] = static_cast<uint32_t>(rs1_value + i.imm);
                        }
                        break;
                    }

                    case F2: // SLTI
                    {
                        if (i.rd != 0) {
                            registers[i.rd] = (rs1_value < i.imm) ? 1U : 0U;
                        }
                        break;
                    }

                    case F3: // SLTIU
                    {
                        if (i.rd != 0) {
                            registers[i.rd] =
                                    (static_cast<uint32_t>(registers[i.rs1]) < static_cast<uint32_t>(i.imm)) ? 1U : 0U;
                        }
                        break;
                    }

                    case F4: // XORI
                    {
                        if (i.rd != 0) {
                            registers[i.rd] = registers[i.rs1] ^ static_cast<uint32_t>(i.imm);
                        }
                        break;
                    }

                    case F6: // ORI
                    {
                        if (i.rd != 0) {
                            registers[i.rd] = registers[i.rs1] | static_cast<uint32_t>(i.imm);
                        }
                        break;
                    }

                    case F7: // ANDI
                    {
                        if (i.rd != 0) {
                            registers[i.rd] = registers[i.rs1] & static_cast<uint32_t>(i.imm);
                        }
                        break;
                    }

                    case F1: // SLLI
                    {
                        uint32_t shift = static_cast<uint32_t>((i.imm & 31));
                        uint32_t funct7 = static_cast<uint32_t>(((i.imm >> 5) & 127));

                        if (funct7 == 0) {
                            if (i.rd != 0) {
                                registers[i.rd] = registers[i.rs1] << shift;
                            }
                        }
                        break;
                    }

                    case F5: {
                        uint32_t shift = static_cast<uint32_t>((i.imm & 31));
                        uint32_t funct7 = static_cast<uint32_t>(((i.imm >> 5) & 127));

                        if (funct7 == 0) {
                            // SRLI
                            if (i.rd != 0) {
                                registers[i.rd] = registers[i.rs1] >> shift;
                            }
                        } else if (funct7 == 32) {
                            // SRAI
                            if (i.rd != 0) {
                                int32_t val = static_cast<int32_t>(registers[i.rs1]);
                                registers[i.rd] = static_cast<uint32_t>((val >> static_cast<int32_t>(shift)));
                            }
                        }
                        break;
                    }
                }
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                break;
            }

            case OPC_23: // AUIPC
            {
                U_Type u = get<U_Type>(type);
                int32_t pc_signed = static_cast<int32_t>(progCount);

                if (u.rd != 0) {
                    registers[u.rd] = static_cast<uint32_t>(pc_signed + (u.imm << 12));
                }
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                break;
            }
            case OPC_35: {
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
            }
            case OPC_51: {
                R_Type r = get<R_Type>(type);
                int32_t srs1 = static_cast<int32_t>(registers[r.rs1]);
                int32_t srs2 = static_cast<int32_t>(registers[r.rs2]);

                switch (r.funct3) {
                    case F0: {
                        switch (r.funct7) {
                            case F0_7: // ADD
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = static_cast<uint32_t>(srs1 + srs2);
                                }
                                break;
                            }

                            case F32_7: // SUB
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = static_cast<uint32_t>(srs1 - srs2);
                                }
                                break;
                            }

                            case F1_7: // MUL
                            {
                                if (r.rd != 0) {
                                    int64_t prod = static_cast<int64_t>(srs1) * static_cast<int64_t>(srs2);
                                    registers[r.rd] = static_cast<uint32_t>(prod);
                                }
                                break;
                            }
                        }

                        break;
                    }

                    case F1:
                        switch (r.funct7) {
                            case F0_7: // SLL
                            {
                                if (r.rd != 0) {
                                    uint32_t shift = registers[r.rs2] & 31;
                                    registers[r.rd] = registers[r.rs1] << shift;
                                }
                                break;
                            }
                            case F1_7: // MULH
                            {
                                if (r.rd != 0) {
                                    int64_t prod = static_cast<int64_t>(srs1) * static_cast<int64_t>(srs2);
                                    registers[r.rd] = static_cast<uint32_t>((prod >> 32));
                                }
                                break;
                            }
                        }
                        break;

                    case F2: {
                        switch (r.funct7) {
                            case F0_7: // SLT
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = (srs1 < srs2) ? 1U : 0U;
                                }
                                break;
                            }

                            case F1_7: // MULHSU
                            {
                                if (r.rd != 0) {
                                    uint64_t big = static_cast<int64_t>(srs1) * static_cast<uint64_t>(registers[r.rs2]);
                                    uint64_t write = static_cast<uint64_t>(big >> 32);
                                    registers[r.rd] = static_cast<uint32_t>(write);
                                }
                                break;
                            }
                        }
                        break;
                    }

                    case F3: {
                        switch (r.funct7) {
                            case F0_7: // SLTU
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = (static_cast<uint32_t>(registers[r.rs1]) <
                                                       static_cast<uint32_t>(registers[r.rs2]))
                                                              ? 1U
                                                              : 0U;
                                }
                                break;
                            }

                            case F1_7: // MULHU
                            {
                                if (r.rd != 0) {
                                    uint64_t big =
                                            static_cast<uint64_t>(srs1) * static_cast<uint64_t>(registers[r.rs2]);
                                    /// вообще может не влезть!!!
                                    uint64_t write = static_cast<uint64_t>(big >> 32);
                                    registers[r.rd] = static_cast<uint32_t>(write);
                                }
                                break;
                            }
                        }

                        break;
                    }

                    case F4: {
                        switch (r.funct7) {
                            case F0_7: // XOR
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = registers[r.rs1] ^ registers[r.rs2];
                                }

                                break;
                            }
                            case F1_7: // DIV
                            {
                                if (r.rd != 0) {
                                    if (registers[r.rs2] != 0) {
                                        registers[r.rd] = static_cast<uint32_t>((srs1 / srs2));
                                    } else {
                                        registers[r.rd] = 4294967295U;
                                    }
                                }
                                break;
                            }
                        }
                        break;
                    }

                    case F5: {
                        switch (r.funct7) {
                            case F0_7: // SRL
                            {
                                if (r.rd != 0) {
                                    uint32_t shift = registers[r.rs2] & 31;
                                    registers[r.rd] = registers[r.rs1] >> shift;
                                }
                                break;
                            }

                            case F1_7: // DIVU
                            {
                                if (r.rd != 0) {
                                    if (registers[r.rs2] != 0) {
                                        registers[r.rd] = registers[r.rs1] / registers[r.rs2];
                                    } else {
                                        registers[r.rd] = 4294967295U; // If rs2 == 0
                                    }
                                }
                                break;
                            }

                            case F32_7: // SRA
                            {
                                if (r.rd != 0) {
                                    uint32_t shift = registers[r.rs2] & 31;
                                    registers[r.rd] = static_cast<uint32_t>((srs1 >> static_cast<int32_t>(shift)));
                                }
                                break;
                            }
                        }
                        break;
                    }

                    case F6: {
                        switch (r.funct7) {
                            case F0_7: // OR
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = registers[r.rs1] | registers[r.rs2];
                                }

                                break;
                            }
                            case F1_7: // REM
                            {
                                if (r.rd != 0) {
                                    if (registers[r.rs2] != 0) {
                                        registers[r.rd] = static_cast<uint32_t>((srs1 % srs2));
                                    } else {
                                        registers[r.rd] = registers[r.rs1];
                                    }
                                }

                                break;
                            }
                        }
                        break;
                    }

                    case F7: {
                        switch (r.funct7) {
                            case F0_7: // AND
                            {
                                if (r.rd != 0) {
                                    registers[r.rd] = registers[r.rs1] & registers[r.rs2];
                                }
                                break;
                            }
                            case F1_7: // REMU
                            {
                                if (r.rd != 0) {
                                    if (registers[r.rs2] != 0) {
                                        registers[r.rd] = registers[r.rs1] % registers[r.rs2];
                                    } else {
                                        registers[r.rd] = registers[r.rs1];
                                    }
                                }
                                break;
                            }
                        }
                        break;
                    }
                }
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                break;
            }

            case OPC_55: // LUI
            {
                U_Type u = get<U_Type>(type);

                if (u.rd != 0) {
                    registers[u.rd] = (static_cast<uint32_t>(u.imm)) << 12;
                }
                progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                break;
            }

            case OPC_99: {
                B_Type b = get<B_Type>(type);
                bool chahge_counter = false;

                switch (b.funct3) {
                    case F0: // BEQ
                        chahge_counter = (registers[b.rs1] == registers[b.rs2]);
                        break;
                    case F1: // BNE
                        chahge_counter = (registers[b.rs1] != registers[b.rs2]);
                        break;
                    case F4: // BLT
                        chahge_counter =
                                (static_cast<int32_t>(registers[b.rs1]) < static_cast<int32_t>(registers[b.rs2]));
                        break;
                    case F5: // BGE
                        chahge_counter =
                                (static_cast<int32_t>(registers[b.rs1]) >= static_cast<int32_t>(registers[b.rs2]));
                        break;
                    case F6: // BLTU
                        chahge_counter = (registers[b.rs1] < registers[b.rs2]);
                        break;
                    case F7: // BGEU
                        chahge_counter = (registers[b.rs1] >= registers[b.rs2]);
                        break;
                }
                if (chahge_counter) {
                    progCount = static_cast<uint32_t>(static_cast<int32_t>(progCount) + b.imm);
                } else {
                    progCount = static_cast<int32_t>(static_cast<int32_t>(progCount) + 4);
                }
                break;
            }

            case OPC_103: // JALR
            {
                I_Type i = get<I_Type>(type);
                if (i.rd != 0) {
                    registers[i.rd] = progCount + 4;
                }
                progCount = static_cast<uint32_t>(static_cast<int32_t>(registers[i.rs1]) + i.imm) & 4294967294U;
                break;
            }

            case OPC_111: // JAL
            {
                J_Type u = get<J_Type>(type);
                if (u.rd != 0) {
                    registers[u.rd] = progCount + 4;
                }
                progCount = static_cast<uint32_t>((static_cast<int32_t>(progCount) + u.imm));

                break;
            }
            default:;
                // OPC_15 и OPC_115 не нужны
        }
    }

    deque<uint32_t> totalRun(deque<Instruction> instructions) {
        while (true) {
            if (progCount / 4 >= instructions.size()) {
                break;
            }
            runCommand(instructions[progCount / 4]);
        }
        cout << progCount << endl;
        return registers;
    }
};


int main(int argc, char *argv[]) {
    string asm_filename = "no_file";

    for (int i = 1; i < argc; i++) {
        if (static_cast<string>(argv[i]) == "--asm") {
            if (i + 1 < argc) {
                asm_filename = argv[++i];
            }
        }
    }

    Parser parser(asm_filename);
    deque<Instruction> instructions = parser.parse();
    CPU CPU_LRU{};
    auto lru = CPU_LRU.totalRun(instructions);
    for (int i = 0; i < lru.size(); i++) {
        cout << lru[i] << " ";
    }
}
