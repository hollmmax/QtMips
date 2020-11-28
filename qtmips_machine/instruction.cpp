// TODO: Extract Instruction type from opcode.
// #include <fmt/core.h>

using u32 = uint32_t;
using i32 = int32_t;

union Imm {
    struct {
        i32 imm;
    } i;
    struct {
        i32 imm1 : 5;
        i32 imm2 : 27;
    } s;
    struct {
        i32 imm0 : 1;
        i32 imm1 : 4;
        i32 imm2 : 6;
        i32 imm3 : 1;
        i32 imm4 : 20;
    } b;
    struct {
        i32 imm0 : 12;
        i32 imm1 : 20;
    } u;
    struct {
        i32 imm0 : 1;
        i32 imm1 : 10;
        i32 imm2 : 1;
        i32 imm3 : 8;
        i32 imm4 : 12;
    } j;
    u32 raw;

    Imm(u32 data) : raw(data) {}
};
static_assert(
    std::is_standard_layout<Imm>::value,
    "Immediate must be standard layout for bit twidlling to work."
);

enum Tag {
    R, I, S, B, U, J
};

union Ins {
    struct {
        u32 opcode :  7;
        u32 rd     :  5;
        u32 funct3 :  3;
        u32 rs1    :  5;
        u32 rs2    :  5;
        u32 funct7 :  7;
    } r;

    struct {
        u32 opcode :  7;
        u32 rd     :  5;
        u32 funct3 :  3;
        u32 rs1    :  5;
        i32 imm    : 12;
    } i;

    struct {
        u32 opcode :  7;
        i32 imm1   :  5;
        u32 funct3 :  3;
        u32 rs1    :  5;
        u32 rs2    :  5;
        i32 imm2   :  7;
    } s;

    struct {
        u32 opcode :  7;
        i32 imm3   :  1;
        i32 imm1   :  4;
        u32 funct3 :  3;
        u32 rs1    :  5;
        u32 rs2    :  5;
        i32 imm2   :  6;
        i32 imm4   :  1;
    } b;

    struct {
        u32 opcode :  7;
        u32 rd     :  5;
        i32 imm1   : 20;
    } u;

    struct {
        u32 opcode :  7;
        u32 rd     :  5;
        i32 imm3   :  8;
        i32 imm2   :  1;
        i32 imm1   : 10;
        i32 imm4   :  1;
    } j;
    u32 raw;

    Ins(u32 data) : raw(data) {}
    Ins(Tag tag, Imm imm) {
        switch (tag) {
            case R:
                this->raw = 0;
                break;
            case I:
                this->i.imm = imm.i.imm;
                break;
            case S:
                this->s.imm1 = imm.s.imm1;
                this->s.imm2 = imm.s.imm2;
                break;
            case B:
                this->b.imm1 = imm.b.imm1;
                this->b.imm2 = imm.b.imm2;
                this->b.imm3 = imm.b.imm3;
                this->b.imm4 = imm.b.imm3;
                break;
            case U:
                this->u.imm1 = imm.u.imm1;
                break;
            case J:
                this->j.imm1 = imm.j.imm1;
                this->j.imm2 = imm.j.imm2;
                this->j.imm3 = imm.j.imm3;
                this->j.imm4 = imm.j.imm4;
                break;
            default:
                // This switch is exhaustive, but I don't know how to force C++ to see it...
                this->raw = 0;
                break;
        }
    }

    Imm imm(Tag tag) {
        switch (tag) {
            case R:
                raw = 0;
                break;
            case I:
                i.imm = this->i.imm;
                break;
            case S:
                s.imm1 = this->s.imm1;
                s.imm2 = this->s.imm2;
                break;
            case B:
                b.imm0 = 0;
                b.imm1 = this->b.imm1;
                b.imm2 = this->b.imm2;
                b.imm3 = this->b.imm3;
                b.imm4 = this->b.imm3;
                break;
            case U:
                u.imm0 = 0;
                u.imm1 = this->u.imm1;
                break;
            case J:
                j.imm0 = 0;
                j.imm1 = this->j.imm1;
                j.imm2 = this->j.imm2;
                j.imm3 = this->j.imm3;
                j.imm4 = this->j.imm4;
                break;
            default:
                // This switch is exhaustive, but I don't know how to force C++ to see it...
                raw = 0;
                break;
        }
    }
};
static_assert(
    std::is_standard_layout<Ins>::value,
    "Instruction must be standard layout for bit twidlling to work."
);

// int main() {
//     Ins foo = 0;
//     foo.r.opcode = 0b1;
//     fmt::print("{}\n", sizeof(Ins) * 8);
//     fmt::print("{:032b}\n", foo.raw);
//     foo.i.imm = ~0;
//     fmt::print("{:032b}\n", foo.raw);
//     Imm bar = 0;
//     bar.i.imm = foo.i.imm;
//     fmt::print("{:032b}\n", bar.raw);
//     return 0;
// }
