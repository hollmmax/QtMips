#ifndef src/machine/stages_h_INCLUDED
#define src/machine/stages_h_INCLUDED

#include "memory/address.h"
#include "instruction.h"

#include <stdint.h>

namespace machine {

struct pipeline{
    enum ForwardFrom {
        FORWARD_NONE = 0b00,
        FORWARD_FROM_W = 0b01,
        FORWARD_FROM_M = 0b10,
    };
    struct dtFetch {
        Instruction inst;  // Loaded instruction
        Address inst_addr; // Address of instruction
        enum ExceptionCause excause;
        bool in_delay_slot;
        bool is_valid;
    };
    struct dtDecode {
        Instruction inst;
        bool memread;              // If memory should be read
        bool memwrite;             // If memory should write input
        bool alusrc;               // If second value to alu is immediate value (rt used
                                   // otherwise)
        bool regd;                 // If rd is used (otherwise rt is used for write target)
        bool regd31;               // Use R31 as destionation for JAL
        bool regwrite;             // If output should be written back to register (which
                                   // one depends on regd)
        bool alu_req_rs;          // requires rs value for ALU
        bool alu_req_rt;          // requires rt value for ALU or SW
        bool bjr_req_rs;          // requires rs for beq, bne, blez, bgtz, jr nad jalr
        bool bjr_req_rt;          // requires rt for beq, bne
        bool branch;               // branch instruction
        bool jump;                 // jump
        bool bj_not;               // negate branch condition
        bool bgt_blez;             // BGTZ/BLEZ instead of BGEZ/BLTZ
        bool nb_skip_ds;           // Skip delay slot if branch is not taken
        bool forward_m_d_rs;       // forwarding required for beq, bne, blez, bgtz, jr
                                   // nad jalr
        bool forward_m_d_rt;       // forwarding required for beq, bne
        enum AluOp aluop;          // Decoded ALU operation
        enum AccessControl memctl; // Decoded memory access type
        uint8_t num_rs;           // Number of the register s1
        uint8_t num_rt;           // Number of the register s2
        uint8_t num_rd;            // Number of the register d
        RegisterValue val_rs;     // Value from register rs
        RegisterValue val_rt;     // Value from register rt
        uint32_t immediate_val;    // Sign-extended immediate value
        uint8_t rwrite;            // Writeback register (multiplexed between rt and
                                   // rd according to regd)
        ForwardFrom ff_rs;
        ForwardFrom ff_rt;
        Address inst_addr;         // Address of instruction
        enum ExceptionCause excause;
        bool in_delay_slot;
        bool stall;
        bool stop_if;
        bool is_valid;
        bool alu_mod; // alternative versions of ADD and right-shift
    };
    struct dtExecute {
        Instruction inst;
        bool memread;
        bool memwrite;
        bool regwrite;
        enum AccessControl memctl;
        RegisterValue val_rt;
        uint8_t rwrite;
        // Writeback register (multiplexed between rt and rd according to regd)
        RegisterValue alu_val; // Result of ALU execution
        Address inst_addr;     // Address of instruction
        enum ExceptionCause excause;
        bool in_delay_slot;
        bool stop_if;
        bool is_valid;
    };
    struct dtMemory {
        Instruction inst;
        bool memtoreg;
        bool regwrite;
        uint8_t rwrite;
        RegisterValue towrite_val;
        Address mem_addr;  // Address used to access memory
        Address inst_addr; // Address of instruction
        enum ExceptionCause excause;
        bool in_delay_slot;
        bool stop_if;
        bool is_valid;
    };

    struct dtFetch fetch;
    struct dtDecode decode;
    struct dtExecute execute;
    struct dtMemory memory;
};
}

#endif // src/machine/stages_h_INCLUDED

