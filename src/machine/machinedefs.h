#ifndef MACHINEDEFS_H
#define MACHINEDEFS_H

#include "memory/address.h"

#include <cstdint>
#include <qmetatype.h>

namespace machine {

enum AccessControl {
    AC_NONE,
    AC_I8,
    AC_U8,
    AC_I16,
    AC_U16,
    AC_I32,
    AC_U32,
    AC_I64,
    AC_U64,
    AC_LOAD_LINKED,
    AC_STORE_CONDITIONAL,
    AC_WORD_RIGHT,
    AC_WORD_LEFT,
    AC_CACHE_OP,
};

constexpr AccessControl AC_FIRST_REGULAR = AC_I8;
constexpr AccessControl AC_LAST_REGULAR = AC_U64;
constexpr AccessControl AC_FIRST_SPECIAL = AC_LOAD_LINKED;
constexpr AccessControl AC_LAST_SPECIAL = AC_CACHE_OP;

constexpr bool is_regular_access(AccessControl type) {
    return AC_FIRST_REGULAR <= type and type <= AC_LAST_REGULAR;
}

constexpr bool is_special_access(AccessControl type) {
    return AC_FIRST_SPECIAL <= type and type <= AC_LAST_SPECIAL;
}
static_assert(is_special_access(AC_CACHE_OP), "");
static_assert(is_special_access((AccessControl)13), "");

enum ExceptionCause {
    EXCAUSE_NONE = 0, // Use zero as default value when no exception is
    // pending.
    EXCAUSE_INT = 1, // Int is 0 on real CPU and in Cause register.
    EXCAUSE_UNKNOWN = 2,
    EXCAUSE_ADDRL = 4,
    EXCAUSE_ADDRS = 5,
    EXCAUSE_IBUS = 6,
    EXCAUSE_DBUS = 7,
    EXCAUSE_SYSCALL = 8,
    EXCAUSE_BREAK = 9,
    EXCAUSE_OVERFLOW = 12,
    EXCAUSE_TRAP = 13,
    EXCAUSE_HWBREAK = 14,
    EXCAUSE_COUNT = 15,
};

// enum AluOp : uint8_t {
//     ALU_OP_NOP,
//     ALU_OP_SLL,
//     ALU_OP_SRL,
//     ALU_OP_ROTR,
//     ALU_OP_SRA,
//     ALU_OP_SLLV,
//     ALU_OP_SRLV,
//     ALU_OP_ROTRV,
//     ALU_OP_SRAV,
//     ALU_OP_MOVZ,
//     ALU_OP_MOVN,
//     ALU_OP_MFHI,
//     ALU_OP_MTHI,
//     ALU_OP_MFLO,
//     ALU_OP_MTLO,
//     ALU_OP_MULT,
//     ALU_OP_MULTU,
//     ALU_OP_DIV,
//     ALU_OP_DIVU,
//     ALU_OP_ADD,
//     ALU_OP_ADDU,
//     ALU_OP_SUB,
//     ALU_OP_SUBU,
//     ALU_OP_AND,
//     ALU_OP_OR,
//     ALU_OP_XOR,
//     ALU_OP_NOR,
//     ALU_OP_SLT,
//     ALU_OP_SLTU,
//     ALU_OP_MUL,
//     ALU_OP_MADD,
//     ALU_OP_MADDU,
//     ALU_OP_MSUB,
//     ALU_OP_MSUBU,
//     ALU_OP_TGE,
//     ALU_OP_TGEU,
//     ALU_OP_TLT,
//     ALU_OP_TLTU,
//     ALU_OP_TEQ,
//     ALU_OP_TNE,
//     ALU_OP_LUI,
//     ALU_OP_WSBH,
//     ALU_OP_SEB,
//     ALU_OP_SEH,
//     ALU_OP_EXT,
//     ALU_OP_INS,
//     ALU_OP_CLZ,
//     ALU_OP_CLO,
//     ALU_OP_PASS_T, // Pass t argument without change for JAL
//     ALU_OP_BREAK,
//     ALU_OP_SYSCALL,
//     ALU_OP_RDHWR,
//     ALU_OP_MTC0,
//     ALU_OP_MFC0,
//     ALU_OP_MFMC0,
//     ALU_OP_ERET,
//     ALU_OP_UNKNOWN,
//     ALU_OP_LAST // First impossible operation (just to be sure that we don't
//     // overflow)
// };

enum LocationStatus {
    LOCSTAT_NONE = 0,
    LOCSTAT_CACHED = 1 << 0,
    LOCSTAT_DIRTY = 1 << 1,
    LOCSTAT_READ_ONLY = 1 << 2,
    LOCSTAT_ILLEGAL = 1 << 3,
};

const Address STAGEADDR_NONE = 0xffffffff_addr;
} // namespace machine

Q_DECLARE_METATYPE(machine::AccessControl)

#endif // MACHINEDEFS_H
