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
    Instruction inst = Instruction::NOP;  // Loaded instruction
    Address inst_addr = 0_addr; // Address of instruction
    enum ExceptionCause excause = EXCAUSE_NONE;
    bool in_delay_slot = false;
    bool is_valid = true;
};
struct FetchInternalState {
    RegisterValue fetched_value = 0;
    unsigned excause_num = 0;
};
struct DecodeInterstage {
    Instruction inst = Instruction::NOP;
    bool memread = false;    // If memory should be read
    bool memwrite = false;   // If memory should write input
    bool alusrc = false;     // If second value to alu is immediate value (rt used
                     // otherwise)
    bool regd = true;       // If rd is used (otherwise rt is used for write 
                         // target)
    bool regwrite = false;   // If output should be written back to register (which
                     // one depends on regd)
    bool alu_req_rs = false; // requires rs value for ALU
    bool alu_req_rt = false; // requires rt value for ALU or SW
    bool bjr_req_rs = false; // requires rs for beq, bne, blez, bgtz, jr nad jalr
    bool bjr_req_rt = false; // requires rt for beq, bne
    bool branch = false;     // branch instruction
    bool jump = false;       // jump
    bool bj_not = false;     // negate branch condition
    bool bgt_blez = false;   // BGTZ/BLEZ instead of BGEZ/BLTZ
    bool nb_skip_ds = false; // Skip delay slot if branch is not taken
    bool forward_m_d_rs = false; // forwarding required for beq, bne, blez, bgtz, jr
                         // nad jalr
    bool forward_m_d_rt = false; // forwarding required for beq, bne
    enum AluOp aluop;    // Decoded ALU operation
    enum AccessControl memctl;   // Decoded memory access type
    uint8_t num_rs1 = 0;         // Number of the register s1
    uint8_t num_rs2 = 0;         // Number of the register s2
    uint8_t num_rd = 0;          // Number of the register d
    uint8_t wb_num_rd = 0;       // Writeback register (multiplexed between rt
                                 // and
    RegisterValue val_rs = 0;        // Value from register rs
    RegisterValue val_rs1_orig = 0;  // Value from register rs1 without forwarding
    RegisterValue val_rt = 0;        // Value from register rt
    RegisterValue val_rs2_orig = 0;  // Value from register rs1 without forwarding
    RegisterValue immediate_val = 0; // Sign-extended immediate value
                                 // rd according to regd)
    ForwardFrom ff_rs1 = FORWARD_NONE;
    ForwardFrom ff_rs2 = FORWARD_NONE;
    Address inst_addr = 0_addr; // Address of instruction
    enum ExceptionCause excause = EXCAUSE_NONE;
    bool in_delay_slot = false;
    bool stall = false;
    bool stop_if = false;
    bool is_valid = false;
    bool alu_mod = false; // alternative versions of ADD and right-shift
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
    bool memread = false;
    bool memwrite = false;
    bool regwrite = false;
    enum AccessControl memctl;
    RegisterValue val_rt = 0;
    uint8_t num_rd = 0;
    // Writeback register (multiplexed between rt and rd according to regd)
    RegisterValue alu_val = 0; // Result of ALU execution
    Address inst_addr = 0_addr;     // Address of instruction
    enum ExceptionCause excause = EXCAUSE_NONE;
    bool in_delay_slot = false;
    bool stop_if = false;
    bool is_valid = false;
};
struct ExecuteInternalState {
    bool alu_src = false;
    bool alu_zero = false;
    bool branch = false;
    RegisterValue alu_src1 = 0;
    RegisterValue alu_src2 = 0;
    RegisterValue immediate = 0;
    RegisterValue rs1 = 0;
    RegisterValue rs2 = 0;
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
    bool memtoreg = false;
    bool regwrite = false;
    uint8_t num_rd = 0;
    RegisterValue towrite_val = 0;
    Address mem_addr = 0_addr;  // Address used to access memory
    Address inst_addr = 0_addr; // Address of instruction
    enum ExceptionCause excause = EXCAUSE_NONE;
    bool in_delay_slot = false;
    bool stop_if = false;
    bool is_valid = false;
};
struct MemoryInternalState {
    bool memwrite = false;
    bool memread = false;
    RegisterValue mem_read_val = 0;
    RegisterValue mem_write_val = 0;
    unsigned excause_num = 0;
};
struct WritebackInternalState {
    Instruction inst = Instruction::NOP;
    Address inst_addr = 0_addr;
    bool regwrite = false;
};
struct FetchState {
    FetchInternalState internal {};
    FetchInterstage result {};
    FetchInterstage final {};

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
    DecodeInternalState internal {};
    DecodeInterstage result {};
    DecodeInterstage final {};

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
    ExecuteInternalState internal {};
    ExecuteInterstage result {};
    ExecuteInterstage final {};

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
    MemoryInternalState internal {};
    MemoryInterstage result {};
    MemoryInterstage final {};

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
    FetchState fetch {};
    DecodeState decode {};
    ExecuteState execute {};
    MemoryState memory {};
    WritebackState writeback {};
};
} // namespace machine

#endif // STAGES_H
