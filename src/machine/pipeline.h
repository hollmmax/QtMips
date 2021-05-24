/**
 * State of the core pipeline.
 *
 * Each internal has a state struct. The state struct is composed of the
 * internal state for visualization and two sets of outgoing interstage
 * registers - result and final. Both are filled with the same data as the
 * stage finishes, but result is never later modified. Final will be modified
 * by flushed, exceptions etc. and it will be used for further execution.
 *
 * TODO:
 * - Move init functions here as methods (constructor/discard).
 *
 * @file
 */
#ifndef STAGES_H
#define STAGES_H

#include "instruction.h"
#include "memory/address.h"

#include <cstdint>
#include <utility>

namespace machine {

enum ForwardFrom {
    FORWARD_NONE = 0b00,
    FORWARD_FROM_W = 0b01,
    FORWARD_FROM_M = 0b10,
};
struct FetchInterstage {
    Instruction inst;  // Loaded instruction
    Address inst_addr; // Address of instruction
    enum ExceptionCause excause;
    bool in_delay_slot;
    bool is_valid;
};
struct FetchInternalState {
    RegisterValue fetched_value;
    unsigned excause_num = 0;
};
struct DecodeInterstage {
    Instruction inst;
    bool memread;    // If memory should be read
    bool memwrite;   // If memory should write input
    bool alusrc;     // If second value to alu is immediate value (rt used
                     // otherwise)
    bool regd;       // If rd is used (otherwise rt is used for write target)
    bool regwrite;   // If output should be written back to register (which
                     // one depends on regd)
    bool alu_req_rs; // requires rs value for ALU
    bool alu_req_rt; // requires rt value for ALU or SW
    bool bjr_req_rs; // requires rs for beq, bne, blez, bgtz, jr nad jalr
    bool bjr_req_rt; // requires rt for beq, bne
    bool branch;     // branch instruction
    bool jump;       // jump
    bool bj_not;     // negate branch condition
    bool bgt_blez;   // BGTZ/BLEZ instead of BGEZ/BLTZ
    bool nb_skip_ds; // Skip delay slot if branch is not taken
    bool forward_m_d_rs; // forwarding required for beq, bne, blez, bgtz, jr
                         // nad jalr
    bool forward_m_d_rt; // forwarding required for beq, bne
    enum AluOp aluop;    // Decoded ALU operation
    enum AccessControl memctl;   // Decoded memory access type
    uint8_t num_rs1 = 0;         // Number of the register s1
    uint8_t num_rs2 = 0;         // Number of the register s2
    uint8_t num_rd = 0;          // Number of the register d
    uint8_t wb_num_rd = 0;       // Writeback register (multiplexed between rt
                                 // and
    RegisterValue val_rs;        // Value from register rs
    RegisterValue val_rs1_orig;  // Value from register rs1 without forwarding
    RegisterValue val_rt;        // Value from register rt
    RegisterValue val_rs2_orig;  // Value from register rs1 without forwarding
    RegisterValue immediate_val; // Sign-extended immediate value
                                 // rd according to regd)
    ForwardFrom ff_rs1;
    ForwardFrom ff_rs2;
    Address inst_addr; // Address of instruction
    enum ExceptionCause excause;
    bool in_delay_slot;
    bool stall;
    bool stop_if;
    bool is_valid;
    bool alu_mod; // alternative versions of ADD and right-shift
};
struct DecodeInternalState {
    /**
     * ALU OP as a number
     * GUI needs to show a number, not enumerated value (for simple interface).
     * Core is responsive for the conversion.
     */
    unsigned alu_op_num = 0;
    unsigned excause_num = 0;
};
struct ExecuteInterstage {
    Instruction inst;
    bool memread;
    bool memwrite;
    bool regwrite;
    enum AccessControl memctl;
    RegisterValue val_rt;
    uint8_t num_rd = 0;
    // Writeback register (multiplexed between rt and rd according to regd)
    RegisterValue alu_val; // Result of ALU execution
    Address inst_addr;     // Address of instruction
    enum ExceptionCause excause;
    bool in_delay_slot;
    bool stop_if;
    bool is_valid;
};
struct ExecuteInternalState {
    bool alu_src;
    bool alu_zero;
    bool branch;
    RegisterValue alu_src1 { 0 };
    RegisterValue alu_src2 { 0 };
    RegisterValue immediate { 0 };
    RegisterValue rs1 { 0 };
    RegisterValue rs2 { 0 };
    unsigned stall_status = 0;
    /**
     * ALU OP as a number.
     * GUI needs to show a number, not enumerated value (for simple interface).
     * Core is responsive for the conversion.
     */
    unsigned alu_op_num = 0;
    /**
     * Forwarding setting as a number.
     * Same note as alu_op_num.
     */
    unsigned forward_from_rs1_num = 0;
    /**
     * Forwarding setting as a number.
     * Same note as alu_op_num.
     */
    unsigned forward_from_rs2_num = 0;
    unsigned excause_num = 0;
};
struct MemoryInterstage {
    Instruction inst;
    bool memtoreg;
    bool regwrite;
    uint8_t num_rd = 0;
    RegisterValue towrite_val;
    Address mem_addr;  // Address used to access memory
    Address inst_addr; // Address of instruction
    enum ExceptionCause excause;
    bool in_delay_slot;
    bool stop_if;
    bool is_valid;
};
struct MemoryInternalState {
    bool memwrite;
    bool memread;
    RegisterValue mem_read_val;
    RegisterValue mem_write_val;
    unsigned excause_num = 0;
};
struct WritebackInternalState {
    Instruction inst;
    Address inst_addr;
    bool regwrite;
};
struct FetchState {
    FetchInternalState internal;
    FetchInterstage result;
    FetchInterstage final;

    FetchState(
        const FetchInternalState &stage,
        const FetchInterstage &interstage)
        : internal(stage)
        , result(interstage)
        , final(interstage) {
        this->internal.excause_num = static_cast<unsigned>(interstage.excause);
    }

    FetchState() = default;
    FetchState(const FetchState &) = default;
};
struct DecodeState {
    DecodeInternalState internal;
    DecodeInterstage result;
    DecodeInterstage final;

    DecodeState(
        const DecodeInternalState &stage,
        const DecodeInterstage &interstage)
        : internal(stage)
        , result(interstage)
        , final(interstage) {
        this->internal.excause_num = static_cast<unsigned>(interstage.excause);
        this->internal.alu_op_num = static_cast<unsigned>(interstage.aluop);
    }
    DecodeState() = default;
    DecodeState(const DecodeState &) = default;
};
struct ExecuteState {
    ExecuteInternalState internal;
    ExecuteInterstage result;
    ExecuteInterstage final;

    ExecuteState(
        const ExecuteInternalState &stage,
        const ExecuteInterstage &interstage)
        : internal(stage)
        , result(interstage)
        , final(interstage) {
        this->internal.excause_num = static_cast<unsigned>(interstage.excause);
    }
    ExecuteState() = default;
    ExecuteState(const ExecuteState &) = default;
};
struct MemoryState {
    MemoryInternalState internal;
    MemoryInterstage result;
    MemoryInterstage final;

    MemoryState(
        const MemoryInternalState &stage,
        const MemoryInterstage &interstage)
        : internal(stage)
        , result(interstage)
        , final(interstage) {
        this->internal.excause_num = static_cast<unsigned>(interstage.excause);
    }

    MemoryState() = default;
    MemoryState(const MemoryState &) = default;
};
struct WritebackState {
    WritebackInternalState internal;

    WritebackState(WritebackInternalState stage) : internal(std::move(stage)) {}
    WritebackState() = default;
    WritebackState(const WritebackState &) = default;
};

struct Pipeline {
    FetchState fetch;
    DecodeState decode;
    ExecuteState execute;
    MemoryState memory;
    WritebackState writeback;
};
} // namespace machine

#endif // STAGES_H
