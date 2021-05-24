#include "core.h"

#include "programloader.h"
#include "utils.h"
#include "execute/alu.h"

using namespace machine;

constexpr uint32_t NOP_HEX = 0x00000013;

Core::Core(
    Registers *regs,
    FrontendMemory *mem_program,
    FrontendMemory *mem_data,
    unsigned int min_cache_row_size,
    Cop0State *cop0state)
    : ex_handlers() {
    this->regs = regs;
    this->cop0state = cop0state;
    this->mem_program = mem_program;
    this->mem_data = mem_data;
    this->ex_default_handler = new StopExceptionHandler();
    this->state.min_cache_row_size = min_cache_row_size;
    this->state.hwr_userlocal = 0xe0000000;
    if (cop0state != nullptr) {
        cop0state->setup_core(this);
    }
    for (int i = 0; i < EXCAUSE_COUNT; i++) {
        state.stop_on_exception[i] = true;
        state.step_over_exception[i] = true;
    }
    state.step_over_exception[EXCAUSE_INT] = false;
}

Core::~Core() {
    delete ex_default_handler;
}

void Core::step(bool skip_break) {
    state.cycle_count++;
    emit cycle_c_value(state.cycle_count);
    do_step(skip_break);
    emit step_done();
}

void Core::reset() {
    state.cycle_count = 0;
    state.stall_count = 0;
    do_reset();
}

unsigned Core::get_cycle_count() const {
    return state.cycle_count;
}

unsigned Core::get_stall_count() const {
    return state.stall_count;
}

Registers *Core::get_regs() {
    return regs;
}

Cop0State *Core::get_cop0state() {
    return cop0state;
}

FrontendMemory *Core::get_mem_data() {
    return mem_data;
}

FrontendMemory *Core::get_mem_program() {
    return mem_program;
}

void Core::insert_hwbreak(Address address) {
    state.hw_breaks.insert(address, new hwBreak(address));
}

void Core::remove_hwbreak(Address address) {
    hwBreak *hwbrk = state.hw_breaks.take(address);
    delete hwbrk;
}

bool Core::is_hwbreak(Address address) {
    hwBreak *hwbrk = state.hw_breaks.value(address);
    return hwbrk != nullptr;
}

void Core::set_stop_on_exception(enum ExceptionCause excause, bool value) {
    state.stop_on_exception[excause] = value;
}

bool Core::get_stop_on_exception(enum ExceptionCause excause) const {
    return state.stop_on_exception[excause];
}

void Core::set_step_over_exception(enum ExceptionCause excause, bool value) {
    state.step_over_exception[excause] = value;
}

bool Core::get_step_over_exception(enum ExceptionCause excause) const {
    return state.step_over_exception[excause];
}

void Core::register_exception_handler(
    ExceptionCause excause,
    ExceptionHandler *exhandler) {
    if (excause == EXCAUSE_NONE) {
        delete ex_default_handler;
        ex_default_handler = exhandler;
    } else {
        ExceptionHandler *old = ex_handlers.take(excause);
        delete old;
        ex_handlers.insert(excause, exhandler);
    }
}

bool Core::handle_exception(
    Core *core,
    Registers *regs,
    ExceptionCause excause,
    Address inst_addr,
    Address next_addr,
    Address jump_branch_pc,
    bool in_delay_slot,
    Address mem_ref_addr) {
    bool ret = false;
    if (excause == EXCAUSE_HWBREAK) {
        if (in_delay_slot) {
            regs->pc_abs_jmp(jump_branch_pc);
        } else {
            regs->pc_abs_jmp(inst_addr);
        }
    }

    if (cop0state != nullptr) {
        if (in_delay_slot) {
            cop0state->write_cop0reg(Cop0State::EPC, jump_branch_pc.get_raw());
        } else {
            cop0state->write_cop0reg(Cop0State::EPC, inst_addr.get_raw());
        }
        cop0state->update_execption_cause(excause, in_delay_slot);
        if (cop0state->read_cop0reg(Cop0State::EBase) != 0
            && !get_step_over_exception(excause)) {
            cop0state->set_status_exl(true);
            regs->pc_abs_jmp(cop0state->exception_pc_address());
        }
    }

    ExceptionHandler *exhandler = ex_handlers.value(excause);
    if (exhandler != nullptr) {
        ret = exhandler->handle_exception(
            core, regs, excause, inst_addr, next_addr, jump_branch_pc,
            in_delay_slot, mem_ref_addr);
    } else if (ex_default_handler != nullptr) {
        ret = ex_default_handler->handle_exception(
            core, regs, excause, inst_addr, next_addr, jump_branch_pc,
            in_delay_slot, mem_ref_addr);
    }
    if (get_stop_on_exception(excause)) {
        emit core->stop_on_exception_reached();
    }

    return ret;
}

void Core::set_c0_userlocal(uint32_t address) {
    state.hwr_userlocal = address;
    if (cop0state != nullptr) {
        if (address != cop0state->read_cop0reg(Cop0State::UserLocal)) {
            cop0state->write_cop0reg(Cop0State::UserLocal, address);
        }
    }
}

enum ExceptionCause Core::memory_special(
    enum AccessControl memctl,
    int mode,
    bool memread,
    bool memwrite,
    RegisterValue &towrite_val,
    RegisterValue rt_value,
    Address mem_addr) {
    UNUSED(mode)

    uint32_t mask;
    uint32_t shift;
    uint32_t temp;

    switch (memctl) {
    case AC_CACHE_OP:
        mem_data->sync();
        mem_program->sync();
        break;
    case AC_STORE_CONDITIONAL:
        if (!memwrite) {
            break;
        }
        mem_data->write_u32(mem_addr, rt_value.as_u32());
        towrite_val = 1;
        break;
    case AC_LOAD_LINKED:
        if (!memread) {
            break;
        }
        towrite_val = mem_data->read_u32(mem_addr);
        break;
    case AC_WORD_RIGHT:
        if (memwrite) {
            shift = (3u - (mem_addr.get_raw() & 3u)) << 3;
            mask = 0xffffffff << shift;
            temp = mem_data->read_u32(mem_addr & ~3u);
            temp = (temp & ~mask) | (rt_value.as_u32() << shift);
            mem_data->write_u32(mem_addr & ~3u, temp);
        } else {
            shift = (3u - (mem_addr.get_raw() & 3u)) << 3;
            mask = 0xffffffff >> shift;
            towrite_val = mem_data->read_u32(mem_addr & ~3u);
            towrite_val
                = (towrite_val.as_u32() >> shift) | (rt_value.as_u32() & ~mask);
        }
        break;
    case AC_WORD_LEFT:
        if (memwrite) {
            shift = (mem_addr.get_raw() & 3u) << 3;
            mask = 0xffffffff >> shift;
            temp = mem_data->read_u32(mem_addr & ~3);
            temp = (temp & ~mask) | (rt_value.as_u32() >> shift);
            mem_data->write_u32(mem_addr & ~3, temp);
        } else {
            shift = (mem_addr.get_raw() & 3u) << 3;
            mask = 0xffffffff << shift;
            towrite_val = mem_data->read_u32(mem_addr & ~3);
            towrite_val
                = (towrite_val.as_u32() << shift) | (rt_value.as_u32() & ~mask);
        }
        break;
    default: break;
    }

    return EXCAUSE_NONE;
}

FetchState Core::fetch(bool skip_break) {
    enum ExceptionCause excause = EXCAUSE_NONE;
    Address inst_addr = Address(regs->read_pc());
    Instruction inst(mem_program->read_u32(inst_addr));

    if (!skip_break) {
        hwBreak *brk = state.hw_breaks.value(inst_addr);
        if (brk != nullptr) {
            excause = EXCAUSE_HWBREAK;
        }
    }
    if (cop0state != nullptr && excause == EXCAUSE_NONE) {
        if (cop0state->core_interrupt_request()) {
            excause = EXCAUSE_INT;
        }
    }

    emit fetch_inst_addr_value(inst_addr);
    emit instruction_fetched(inst, inst_addr, excause, true);
    return { FetchInternalState { .fetched_value = inst.data() },
             FetchInterstage {
                 .inst = inst,
                 .inst_addr = inst_addr,
                 .excause = excause,
                 .in_delay_slot = false,
                 .is_valid = true,
             } };
}

DecodeState Core::decode(const FetchInterstage &dt) {
    uint8_t rwrite;
    enum InstructionFlags flags;
    enum AluOp alu_op;
    enum AccessControl mem_ctl;
    enum ExceptionCause excause = dt.excause;

    dt.inst.flags_alu_op_mem_ctl(flags, alu_op, mem_ctl);

    if (!(flags & IMF_SUPPORTED)) {
        throw SIMULATOR_EXCEPTION(
            UnsupportedInstruction,
            "Instruction with following encoding is not supported",
            QString::number(dt.inst.data(), 16));
    }

    uint8_t num_rs = dt.inst.rs();
    uint8_t num_rt = dt.inst.rt();
    uint8_t num_rd = dt.inst.rd();
    RegisterValue val_rs = regs->read_gp(num_rs);
    RegisterValue val_rt = regs->read_gp(num_rt);
    uint32_t immediate_val;
    bool regwrite = flags & IMF_REGWRITE;
    // bool regd = flags & IMF_REGD;
    bool regd = true; // Rv always writes to rd
    bool regd31 = flags & IMF_PC_TO_R31;

    // requires rs for beq, bne, blez, bgtz, jr nad jalr
    bool bjr_req_rs = flags & IMF_BJR_REQ_RS;
    if (flags & IMF_PC8_TO_RT) {
        val_rt = (dt.inst_addr + 8).get_raw();
    }
    // requires rt for beq, bne
    bool bjr_req_rt = flags & IMF_BJR_REQ_RT;

    // if (flags & IMF_ZERO_EXTEND) {
    //     immediate_val = dt.inst.immediate();
    // } else {
    //     immediate_val = sign_extend(dt.inst.immediate());
    // }
    immediate_val = dt.inst.immediate();

    if ((flags & IMF_EXCEPTION) && (excause == EXCAUSE_NONE)) {
        excause = dt.inst.encoded_exception();
    }

    emit decode_inst_addr_value(dt.inst_addr);
    emit instruction_decoded(dt.inst, dt.inst_addr, excause, dt.is_valid);
    emit decode_instruction_value(dt.inst.data());
    emit decode_reg1_value(val_rs.as_u32());
    emit decode_reg2_value(val_rt.as_u32());
    emit decode_immediate_value(immediate_val);
    emit decode_regw_value(regwrite);
    emit decode_memtoreg_value((bool)(flags & IMF_MEMREAD));
    emit decode_memwrite_value((bool)(flags & IMF_MEMWRITE));
    emit decode_memread_value((bool)(flags & IMF_MEMREAD));
    emit decode_alusrc_value((bool)(flags & IMF_ALUSRC));
    emit decode_regdest_value(regd);
    emit decode_rs_num_value(num_rs);
    emit decode_rt_num_value(num_rt);
    emit decode_rd_num_value(num_rd);
    emit decode_regd31_value(regd31);

    if (regd31) {
        val_rt = (dt.inst_addr + 8).get_raw();
    }

    rwrite = regd31 ? 31 : regd ? num_rd : num_rt;

    return { DecodeInternalState {
                 .alu_op_num = static_cast<unsigned>(alu_op),
             },
             DecodeInterstage {
                 .inst = dt.inst,
                 .memread = !!(flags & IMF_MEMREAD),
                 .memwrite = !!(flags & IMF_MEMWRITE),
                 .alusrc = !!(flags & IMF_ALUSRC),
                 .regd = regd,
                 .regwrite = regwrite,
                 .alu_req_rs = !!(flags & IMF_ALU_REQ_RS),
                 .alu_req_rt = !!(flags & IMF_ALU_REQ_RT),
                 .bjr_req_rs = bjr_req_rs,
                 .bjr_req_rt = bjr_req_rt,
                 .branch = !!(flags & IMF_BRANCH),
                 .jump = !!(flags & IMF_JUMP),
                 .bj_not = !!(flags & IMF_BJ_NOT),
                 .bgt_blez = !!(flags & IMF_BGTZ_BLEZ),
                 .nb_skip_ds = !!(flags & IMF_NB_SKIP_DS),
                 .forward_m_d_rs = false,
                 .forward_m_d_rt = false,
                 .aluop = alu_op,
                 .memctl = mem_ctl,
                 .num_rs1 = num_rs,
                 .num_rs2 = num_rt,
                 .num_rd = num_rd,
                 .wb_num_rd = rwrite,
                 .val_rs = val_rs,
                 .val_rs1_orig = val_rs,
                 .val_rt = val_rt,
                 .val_rs2_orig = val_rt,
                 .immediate_val = immediate_val,
                 .ff_rs1 = FORWARD_NONE,
                 .ff_rs2 = FORWARD_NONE,
                 .inst_addr = dt.inst_addr,
                 .excause = excause,
                 .in_delay_slot = dt.in_delay_slot,
                 .stall = false,
                 .stop_if = !!(flags & IMF_STOP_IF),
                 .is_valid = dt.is_valid,
                 .alu_mod = bool(flags & IMF_ALU_MOD),
             } };
}

ExecuteState Core::execute(const DecodeInterstage &dt) {
    bool discard;
    enum ExceptionCause excause = dt.excause;
    RegisterValue alu_val = 0;

    // Handle conditional move (we have to change regwrite signal if conditional
    // is not met)
    bool regwrite = dt.regwrite;

    RegisterValue alu_sec = dt.val_rt;
    if (dt.alusrc) {
        alu_sec = dt.immediate_val; // Sign or zero extend immediate value
    }

    if (excause == EXCAUSE_NONE) {
        alu_val = alu_combined_operate({.alu_op = dt.aluop}, AluComponent::ALU, true, dt.alu_mod, dt.val_rs, alu_sec);
        if (dt.branch) alu_val = dt.bj_not ^ (alu_val == 0);
        discard = dt.num_rd == 0;
        if (discard) {
            regwrite = false;
        }

        // switch (dt.aluop) {
        // case ALU_OP_RDHWR:
        //     switch (dt.num_rd) {
        //     case 0: // CPUNum
        //         alu_val = 0;
        //         break;
        //     case 1: // SYNCI_Step
        //         alu_val = min_cache_row_size;
        //         break;
        //     case 2: // CC
        //         alu_val = state.cycle_count;
        //         break;
        //     case 3: // CCRes
        //         alu_val = 1;
        //         break;
        //     case 29: // UserLocal
        //         alu_val = hwr_userlocal;
        //         break;
        //     default: alu_val = 0;
        //     }
        //     break;
        // case ALU_OP_MTC0:
        //     if (cop0state == nullptr) {
        //         throw SIMULATOR_EXCEPTION(
        //             UnsupportedInstruction, "Cop0 not supported",
        //             "setup Cop0State");
        //     }
        //     cop0state->write_cop0reg(dt.num_rd, dt.inst.cop0sel(),
        //     dt.val_rt); break;
        // case ALU_OP_MFC0:
        //     if (cop0state == nullptr) {
        //         throw SIMULATOR_EXCEPTION(
        //             UnsupportedInstruction, "Cop0 not supported",
        //             "setup Cop0State");
        //     }
        //     alu_val = cop0state->read_cop0reg(dt.num_rd, dt.inst.cop0sel());
        //     break;
        // case ALU_OP_MFMC0:
        //     if (cop0state == nullptr) {
        //         throw SIMULATOR_EXCEPTION(
        //             UnsupportedInstruction, "Cop0 not supported",
        //             "setup Cop0State");
        //     }
        //     alu_val = cop0state->read_cop0reg(dt.num_rd, dt.inst.cop0sel());
        //     if (dt.inst.funct() & 0x20) {
        //         cop0state->write_cop0reg(
        //             dt.num_rd, dt.inst.cop0sel(), dt.val_rt.as_u32() | 1);
        //     } else {
        //         cop0state->write_cop0reg(
        //             dt.num_rd, dt.inst.cop0sel(), dt.val_rt.as_u32() & ~1U);
        //     }
        //     break;
        // case ALU_OP_ERET:
        //     regs->pc_abs_jmp(Address(cop0state->read_cop0reg(Cop0State::EPC)));
        //     if (cop0state != nullptr) {
        //         cop0state->set_status_exl(false);
        //     }
        //     break;
        // default: break;
        // }
    }

    emit execute_inst_addr_value(dt.inst_addr);
    emit instruction_executed(dt.inst, dt.inst_addr, excause, dt.is_valid);
    emit execute_alu_value(alu_val.as_u32());
    emit execute_reg1_value(dt.val_rs.as_u32());
    emit execute_reg2_value(dt.val_rt.as_u32());
    emit execute_reg1_ff_value(dt.ff_rs1);
    emit execute_reg2_ff_value(dt.ff_rs2);
    //    emit execute_immediate_value(dt.immediate_val);
    emit execute_regw_value(dt.regwrite);
    emit execute_memtoreg_value(dt.memread);
    emit execute_memread_value(dt.memread);
    emit execute_memwrite_value(dt.memwrite);
    emit execute_alusrc_value(dt.alusrc);
    emit execute_regdest_value(dt.regd);
    emit execute_regw_num_value(dt.wb_num_rd);
    emit execute_rs_num_value(dt.num_rs1);
    emit execute_rt_num_value(dt.num_rs2);
    emit execute_rd_num_value(dt.num_rd);

    const unsigned stall_status = [&]() {
        if (dt.stall) {
            return 1;
        } else if (dt.ff_rs1 != FORWARD_NONE || dt.ff_rs2 != FORWARD_NONE) {
            return 2;
        } else {
            return 0;
        }
    }();
    emit execute_stall_forward_value(stall_status);

    return { ExecuteInternalState {
                 .alu_src = dt.alusrc,
                 .alu_zero = alu_val == 0,
                 .branch = dt.branch,
                 .alu_src1 = dt.val_rs,
                 .alu_src2 = alu_sec,
                 .immediate = dt.immediate_val,
                 .rs1 = dt.val_rs1_orig,
                 .rs2 = dt.val_rs2_orig,
                 .stall_status = stall_status,
                 .alu_op_num = static_cast<unsigned>(dt.aluop),
                 .forward_from_rs1_num = static_cast<unsigned>(dt.ff_rs1),
                 .forward_from_rs2_num = static_cast<unsigned>(dt.ff_rs2),
                 .excause_num = static_cast<unsigned>(dt.excause),
             },
             ExecuteInterstage {
                 .inst = dt.inst,
                 .memread = dt.memread,
                 .memwrite = dt.memwrite,
                 .regwrite = regwrite,
                 .memctl = dt.memctl,
                 .val_rt = dt.val_rt,
                 .num_rd = dt.wb_num_rd,
                 .alu_val = alu_val,
                 .inst_addr = dt.inst_addr,
                 .excause = excause,
                 .in_delay_slot = dt.in_delay_slot,
                 .stop_if = dt.stop_if,
                 .is_valid = dt.is_valid,
             } };
}

MemoryState Core::memory(const ExecuteInterstage &dt) {
    RegisterValue towrite_val = dt.alu_val;
    Address mem_addr = Address(dt.alu_val.as_u32());
    bool memread = dt.memread;
    bool memwrite = dt.memwrite;
    bool regwrite = dt.regwrite;

    enum ExceptionCause excause = dt.excause;
    if (excause == EXCAUSE_NONE) {
        if (is_special_access(dt.memctl)) {
            excause = memory_special(
                dt.memctl, dt.inst.rt(), memread, memwrite, towrite_val,
                dt.val_rt, mem_addr);
        } else if (is_regular_access(dt.memctl)) {
            if (memwrite) {
                mem_data->write_ctl(dt.memctl, mem_addr, dt.val_rt);
            }
            if (memread) {
                towrite_val = mem_data->read_ctl(dt.memctl, mem_addr);
            }
        } else {
            Q_ASSERT(dt.memctl == AC_NONE);
            // AC_NONE is memory NOP
        }
    }

    if (dt.excause != EXCAUSE_NONE) {
        memread = false;
        memwrite = false;
        regwrite = false;
    }

    emit memory_inst_addr_value(dt.inst_addr);
    emit instruction_memory(dt.inst, dt.inst_addr, dt.excause, dt.is_valid);
    emit memory_alu_value(dt.alu_val.as_u32());
    emit memory_rt_value(dt.val_rt.as_u32());
    emit memory_mem_value(memread ? towrite_val.as_u32() : 0);
    emit memory_regw_value(regwrite);
    emit memory_memtoreg_value(dt.memread);
    emit memory_memread_value(dt.memread);
    emit memory_memwrite_value(memwrite);
    emit memory_regw_num_value(dt.num_rd);
    emit memory_excause_value(excause);

    return { MemoryInternalState {
                 .memwrite = dt.memwrite,
                 .memread = dt.memread,
                 .mem_read_val = memread ? towrite_val.as_u32() : 0,
                 .mem_write_val = dt.val_rt,
             },
             MemoryInterstage {
                 .inst = dt.inst,
                 .memtoreg = memread,
                 .regwrite = regwrite,
                 .num_rd = dt.num_rd,
                 .towrite_val = towrite_val,
                 .mem_addr = mem_addr,
                 .inst_addr = dt.inst_addr,
                 .excause = dt.excause,
                 .in_delay_slot = dt.in_delay_slot,
                 .stop_if = dt.stop_if,
                 .is_valid = dt.is_valid,
             } };
}

WritebackState Core::writeback(const MemoryInterstage &dt) {
    emit writeback_inst_addr_value(dt.inst_addr);
    emit instruction_writeback(dt.inst, dt.inst_addr, dt.excause, dt.is_valid);
    emit writeback_value(dt.towrite_val.as_u32());
    emit writeback_memtoreg_value(dt.memtoreg);
    emit writeback_regw_value(dt.regwrite);
    emit writeback_regw_num_value(dt.num_rd);
    if (dt.regwrite) {
        regs->write_gp(dt.num_rd, dt.towrite_val);
    }

    return { WritebackInternalState {
        .inst = dt.inst,
        .inst_addr = dt.inst_addr,
        .regwrite = dt.regwrite,
    } };
}

bool Core::handle_pc(const DecodeInterstage &dt) {
    bool branch = false;
    emit instruction_program_counter(
        dt.inst, dt.inst_addr, EXCAUSE_NONE, dt.is_valid);

    if (dt.jump) {
        if (!dt.bjr_req_rs) {
            regs->pc_abs_jmp_28(dt.inst.address() << 2);
            emit fetch_jump_value(true);
            emit fetch_jump_reg_value(false);
        } else {
            regs->pc_abs_jmp(Address(dt.val_rs.as_u32()));
            emit fetch_jump_value(false);
            emit fetch_jump_reg_value(true);
        }
        emit fetch_branch_value(false);
        return true;
    }

    if (dt.branch) {
        if (dt.bjr_req_rt) {
            branch = dt.val_rs.as_u32() == dt.val_rt.as_u32();
        } else if (!dt.bgt_blez) {
            branch = dt.val_rs.as_i32() < 0;
        } else {
            branch = dt.val_rs.as_i32() <= 0;
        }

        if (dt.bj_not) {
            branch = !branch;
        }
    }

    emit fetch_jump_value(false);
    emit fetch_jump_reg_value(false);
    emit fetch_branch_value(branch);

    if (branch) {
        int32_t rel_offset = dt.inst.immediate() << 2;
        if (rel_offset & (1 << 17)) {
            rel_offset -= 1 << 18;
        }
        regs->pc_abs_jmp(dt.inst_addr + rel_offset + 4);
    } else {
        regs->pc_inc();
    }
    return branch;
}

void Core::dtFetchInit(FetchInterstage &dt) {
    dt.inst = Instruction(NOP_HEX);
    dt.excause = EXCAUSE_NONE;
    dt.in_delay_slot = false;
    dt.is_valid = false;
}

void Core::dtDecodeInit(DecodeInterstage &dt) {
    dt.inst = Instruction(NOP_HEX);
    dt.memread = false;
    dt.memwrite = false;
    dt.alusrc = false;
    dt.regd = false;
    dt.regwrite = false;
    dt.bjr_req_rs = false; // requires rs for beq, bne, blez, bgtz, jr nad
                           // jalr
    dt.bjr_req_rt = false; // requires rt for beq, bne
    dt.jump = false;
    dt.bj_not = false;
    dt.bgt_blez = false;
    dt.nb_skip_ds = false;
    dt.forward_m_d_rs = false;
    dt.forward_m_d_rt = false;
    // dt.aluop = ALU_OP_SLL;
    dt.aluop = AluOp::ADD;
    dt.memctl = AC_NONE;
    dt.num_rs1 = 0;
    dt.num_rs2 = 0;
    dt.num_rd = 0;
    dt.val_rs = 0;
    dt.val_rt = 0;
    dt.wb_num_rd = 0;
    dt.immediate_val = 0;
    dt.ff_rs1 = FORWARD_NONE;
    dt.ff_rs2 = FORWARD_NONE;
    dt.excause = EXCAUSE_NONE;
    dt.in_delay_slot = false;
    dt.stall = false;
    dt.stop_if = false;
    dt.is_valid = false;
}

void Core::dtExecuteInit(ExecuteInterstage &dt) {
    dt.inst = Instruction(NOP_HEX);
    dt.memread = false;
    dt.memwrite = false;
    dt.regwrite = false;
    dt.memctl = AC_NONE;
    dt.val_rt = 0;
    dt.num_rd = 0;
    dt.alu_val = 0;
    dt.excause = EXCAUSE_NONE;
    dt.in_delay_slot = false;
    dt.stop_if = false;
    dt.is_valid = false;
}

void Core::dtMemoryInit(MemoryInterstage &dt) {
    dt.inst = Instruction(NOP_HEX);
    dt.memtoreg = false;
    dt.regwrite = false;
    dt.num_rd = false;
    dt.towrite_val = 0;
    dt.mem_addr = 0x0_addr;
    dt.excause = EXCAUSE_NONE;
    dt.in_delay_slot = false;
    dt.stop_if = false;
    dt.is_valid = false;
}

CoreSingle::CoreSingle(
    Registers *regs,
    FrontendMemory *mem_program,
    FrontendMemory *mem_data,
    unsigned int min_cache_row_size,
    Cop0State *cop0state)
    : Core(regs, mem_program, mem_data, min_cache_row_size, cop0state) {
    reset();
}

void CoreSingle::do_step(bool skip_break) {
    state.pipeline.fetch = fetch(skip_break);
    state.pipeline.decode = decode(state.pipeline.fetch.final);
    state.pipeline.execute = execute(state.pipeline.decode.final);
    state.pipeline.memory = memory(state.pipeline.execute.final);
    state.pipeline.writeback = writeback(state.pipeline.memory.final);

    // Handle PC before instruction following jump leaves decode internal

    {
        bool branch_taken = handle_pc(state.pipeline.decode.final);
    }

    if (state.pipeline.memory.final.excause != EXCAUSE_NONE) {
        handle_exception(
            this, regs, state.pipeline.memory.final.excause,
            state.pipeline.memory.final.inst_addr, regs->read_pc(),
            prev_inst_addr, state.pipeline.memory.final.in_delay_slot,
            state.pipeline.memory.final.mem_addr);
        return;
    }
    prev_inst_addr = state.pipeline.memory.final.inst_addr;
}

void CoreSingle::do_reset() {
    prev_inst_addr = Address::null();
}

CorePipelined::CorePipelined(
    Registers *regs,
    FrontendMemory *mem_program,
    FrontendMemory *mem_data,
    enum MachineConfig::HazardUnit hazard_unit,
    unsigned int min_cache_row_size,
    Cop0State *cop0state)
    : Core(regs, mem_program, mem_data, min_cache_row_size, cop0state) {
    this->hazard_unit = hazard_unit;

    reset();
}

void CorePipelined::do_step(bool skip_break) {
    bool stall = false;
    bool branch_stall = false;
    bool excpt_in_progress;
    Address jump_branch_pc = state.pipeline.memory.final.inst_addr;

    // Process stages
    state.pipeline.writeback = writeback(state.pipeline.memory.final);
    state.pipeline.memory = memory(state.pipeline.execute.final);
    state.pipeline.execute = execute(state.pipeline.decode.final);
    state.pipeline.decode = decode(state.pipeline.fetch.final);

    // Resolve exceptions
    excpt_in_progress = state.pipeline.memory.final.excause != EXCAUSE_NONE;
    if (excpt_in_progress) {
        dtExecuteInit(state.pipeline.execute.final);
        emit instruction_executed(
            state.pipeline.execute.final.inst,
            state.pipeline.execute.final.inst_addr,
            state.pipeline.execute.final.excause,
            state.pipeline.execute.final.is_valid);
        emit execute_inst_addr_value(STAGEADDR_NONE);
    }
    excpt_in_progress = excpt_in_progress
                        || state.pipeline.execute.final.excause != EXCAUSE_NONE;
    if (excpt_in_progress) {
        dtDecodeInit(state.pipeline.decode.final);
        emit instruction_decoded(
            state.pipeline.decode.final.inst,
            state.pipeline.decode.final.inst_addr,
            state.pipeline.decode.final.excause,
            state.pipeline.decode.final.is_valid);
        emit decode_inst_addr_value(STAGEADDR_NONE);
    }
    excpt_in_progress = excpt_in_progress
                        || state.pipeline.execute.final.excause != EXCAUSE_NONE;
    if (excpt_in_progress) {
        dtFetchInit(state.pipeline.fetch.final);
        emit instruction_fetched(
            state.pipeline.fetch.final.inst,
            state.pipeline.fetch.final.inst_addr,
            state.pipeline.fetch.final.excause,
            state.pipeline.fetch.final.is_valid);
        emit fetch_inst_addr_value(STAGEADDR_NONE);
        if (state.pipeline.memory.final.excause != EXCAUSE_NONE) {
            regs->pc_abs_jmp(state.pipeline.execute.final.inst_addr);
            handle_exception(
                this, regs, state.pipeline.memory.final.excause,
                state.pipeline.memory.final.inst_addr,
                state.pipeline.execute.final.inst_addr, jump_branch_pc,
                state.pipeline.memory.final.in_delay_slot,
                state.pipeline.memory.final.mem_addr);
        }
        return;
    }

    state.pipeline.decode.final.ff_rs1 = FORWARD_NONE;
    state.pipeline.decode.final.ff_rs2 = FORWARD_NONE;

    if (hazard_unit != MachineConfig::HU_NONE) {
        // Note: We make exception with $0 as that has no effect when
        // written and is used in nop instruction

#define HAZARD(STAGE)                                                          \
    ((STAGE).final.regwrite && (STAGE).final.num_rd != 0                       \
     && ((state.pipeline.decode.final.alu_req_rs                               \
          && (STAGE).final.num_rd == state.pipeline.decode.final.num_rs1)      \
         || (state.pipeline.decode.final.alu_req_rt                            \
             && (STAGE).final.num_rd                                           \
                    == state.pipeline.decode.final.num_rs2))) //
        // Note:
        // We
        // make
        // exception
        // with
        // $0 as that has no effect and
        // is used in nop instruction

        // Write back internal combinatoricly propagates written instruction to
        // decode internal so nothing has to be done for that internal
        if (HAZARD(state.pipeline.memory)) {
            // Hazard with instruction in memory internal
            if (hazard_unit == MachineConfig::HU_STALL_FORWARD) {
                // Forward result value
                if (state.pipeline.decode.final.alu_req_rs
                    && state.pipeline.memory.final.num_rd
                           == state.pipeline.decode.final.num_rs1) {
                    state.pipeline.decode.final.val_rs
                        = state.pipeline.memory.final.towrite_val;
                    state.pipeline.decode.final.ff_rs1 = FORWARD_FROM_W;
                }
                if (state.pipeline.decode.final.alu_req_rt
                    && state.pipeline.memory.final.num_rd
                           == state.pipeline.decode.final.num_rs2) {
                    state.pipeline.decode.final.val_rt
                        = state.pipeline.memory.final.towrite_val;
                    state.pipeline.decode.final.ff_rs2 = FORWARD_FROM_W;
                }
            } else {
                stall = true;
            }
        }
        if (HAZARD(state.pipeline.execute)) {
            // Hazard with instruction in execute internal
            if (hazard_unit == MachineConfig::HU_STALL_FORWARD) {
                if (state.pipeline.execute.final.memread) {
                    stall = true;
                } else {
                    // Forward result value
                    if (state.pipeline.decode.final.alu_req_rs
                        && state.pipeline.execute.final.num_rd
                               == state.pipeline.decode.final.num_rs1) {
                        state.pipeline.decode.final.val_rs
                            = state.pipeline.execute.final.alu_val;
                        state.pipeline.decode.final.ff_rs1 = FORWARD_FROM_M;
                    }
                    if (state.pipeline.decode.final.alu_req_rt
                        && state.pipeline.execute.final.num_rd
                               == state.pipeline.decode.final.num_rs2) {
                        state.pipeline.decode.final.val_rt
                            = state.pipeline.execute.final.alu_val;
                        state.pipeline.decode.final.ff_rs2 = FORWARD_FROM_M;
                    }
                }
            } else {
                stall = true;
            }
        }
#undef HAZARD
        if (state.pipeline.execute.final.num_rd != 0
            && state.pipeline.execute.final.regwrite
            && ((state.pipeline.decode.final.bjr_req_rs
                 && state.pipeline.decode.final.num_rs1
                        == state.pipeline.execute.final.num_rd)
                || (state.pipeline.decode.final.bjr_req_rt
                    && state.pipeline.decode.final.num_rs2
                           == state.pipeline.execute.final.num_rd))) {
            stall = true;
            branch_stall = true;
        } else {
            if (hazard_unit != MachineConfig::HU_STALL_FORWARD
                || state.pipeline.memory.final.memtoreg) {
                if (state.pipeline.memory.final.num_rd != 0
                    && state.pipeline.memory.final.regwrite
                    && ((state.pipeline.decode.final.bjr_req_rs
                         && state.pipeline.decode.final.num_rs1
                                == state.pipeline.memory.final.num_rd)
                        || (state.pipeline.decode.final.bjr_req_rt
                            && state.pipeline.decode.final.num_rs2
                                   == state.pipeline.memory.final.num_rd))) {
                    stall = true;
                }
            } else {
                if (state.pipeline.memory.final.num_rd != 0
                    && state.pipeline.memory.final.regwrite
                    && state.pipeline.decode.final.bjr_req_rs
                    && state.pipeline.decode.final.num_rs1
                           == state.pipeline.memory.final.num_rd) {
                    state.pipeline.decode.final.val_rs
                        = state.pipeline.memory.final.towrite_val;
                    state.pipeline.decode.final.forward_m_d_rs = true;
                }
                if (state.pipeline.memory.final.num_rd != 0
                    && state.pipeline.memory.final.regwrite
                    && state.pipeline.decode.final.bjr_req_rt
                    && state.pipeline.decode.final.num_rs2
                           == state.pipeline.memory.final.num_rd) {
                    state.pipeline.decode.final.val_rt
                        = state.pipeline.memory.final.towrite_val;
                    state.pipeline.decode.final.forward_m_d_rt = true;
                }
            }
        }
        emit forward_m_d_rs_value(state.pipeline.decode.final.forward_m_d_rs);
        emit forward_m_d_rt_value(state.pipeline.decode.final.forward_m_d_rt);
    }
    emit branch_forward_value(
        (state.pipeline.decode.final.forward_m_d_rs
         || state.pipeline.decode.final.forward_m_d_rt)
            ? 2
            : branch_stall);
#if 0
    if (stall)
        printf("STALL\n");
    else if(state.pipeline.decode.forward_m_d_rs || state.pipeline.decode.forward_m_d_rt)
        printf("f_m_d_rs %d f_m_d_rt %d\n", (int)state.pipeline.decode.forward_m_d_rs, (int)state.pipeline.decode.forward_m_d_rt);
    printf("D: %s inst.type %d state.pipeline.decode.inst.rs [%d] state.pipeline.decode.inst.rt [%d] state.pipeline.decode.ff_rs1 %d state.pipeline.decode.ff_rs2 %d E: regwrite %d inst.type %d  num_rd [%d] M: regwrite %d inst.type %d num_rd [%d] \n",
            state.pipeline.decode.inst.to_str().toLocal8Bit().data(),
            state.pipeline.decode.inst.type(), state.pipeline.decode.inst.rs(), state.pipeline.decode.inst.rt(), state.pipeline.decode.ff_rs1, state.pipeline.decode.ff_rs2,
            state.pipeline.execute.regwrite, state.pipeline.execute.inst.type(), state.pipeline.execute.num_rd,
            state.pipeline.memory.regwrite,  state.pipeline.memory.inst.type(), state.pipeline.memory.num_rd);
#endif
#if 0
    printf("PC 0x%08lx\n", (unsigned long)state.pipeline.fetch.inst_addr);
#endif

    if (state.pipeline.execute.final.stop_if
        || state.pipeline.memory.final.stop_if) {
        stall = true;
    }

    emit hu_stall_value(stall);

    // Now process program counter (loop connections from decode internal)
    if (!stall && !state.pipeline.decode.final.stop_if) {
        state.pipeline.decode.final.stall = false;
        state.pipeline.fetch = fetch(skip_break);
        if (handle_pc(state.pipeline.decode.final)) {
            state.pipeline.fetch.final.in_delay_slot = true;
        } else {
            if (state.pipeline.decode.final.nb_skip_ds) {
                dtFetchInit(state.pipeline.fetch.final);
                emit instruction_fetched(
                    state.pipeline.fetch.final.inst,
                    state.pipeline.fetch.final.inst_addr,
                    state.pipeline.fetch.final.excause,
                    state.pipeline.fetch.final.is_valid);
                emit fetch_inst_addr_value(STAGEADDR_NONE);
            }
        }
    } else {
        // Run fetch internal on empty
        fetch(skip_break);
        // clear decode latch (insert nope to execute internal)
        if (!state.pipeline.decode.final.stop_if) {
            dtDecodeInit(state.pipeline.decode.final);
            state.pipeline.decode.final.stall = true;
        } else {
            dtFetchInit(state.pipeline.fetch.final);
        }
        // emit instruction_decoded(state.pipeline.decode.inst,
        // state.pipeline.decode.inst_addr, state.pipeline.decode.excause,
        // state.pipeline.decode.is_valid);
    }
    if (stall || state.pipeline.decode.final.stop_if) {
        state.stall_count++;
        emit stall_c_value(state.stall_count);
    }
}

void CorePipelined::do_reset() {
    dtFetchInit(state.pipeline.fetch.final);
    dtFetchInit(state.pipeline.fetch.result);
    state.pipeline.fetch.final.inst_addr = 0x0_addr;
    state.pipeline.fetch.result.inst_addr = 0x0_addr;
    dtDecodeInit(state.pipeline.decode.result);
    dtDecodeInit(state.pipeline.decode.final);
    state.pipeline.decode.result.inst_addr = 0x0_addr;
    state.pipeline.decode.final.inst_addr = 0x0_addr;
    dtExecuteInit(state.pipeline.execute.result);
    dtExecuteInit(state.pipeline.execute.final);
    state.pipeline.execute.result.inst_addr = 0x0_addr;
    state.pipeline.execute.final.inst_addr = 0x0_addr;
    dtMemoryInit(state.pipeline.memory.result);
    dtMemoryInit(state.pipeline.memory.final);
    state.pipeline.memory.result.inst_addr = 0x0_addr;
    state.pipeline.memory.final.inst_addr = 0x0_addr;
}

bool StopExceptionHandler::handle_exception(
    Core *core,
    Registers *regs,
    ExceptionCause excause,
    Address inst_addr,
    Address next_addr,
    Address jump_branch_pc,
    bool in_delay_slot,
    Address mem_ref_addr) {
#if 0
    printf("Exception cause %d instruction PC 0x%08lx next PC 0x%08lx jump branch PC 0x%08lx "
           "in_delay_slot %d registers PC 0x%08lx mem ref 0x%08lx\n",
           excause, (unsigned long)inst_addr, (unsigned long)next_addr,
           (unsigned long)jump_branch_pc, (int)in_delay_slot,
           (unsigned long)regs->read_pc(), (unsigned long)mem_ref_addr);
#else
    (void)excause;
    (void)inst_addr;
    (void)next_addr;
    (void)mem_ref_addr;
    (void)regs;
    (void)jump_branch_pc;
    (void)in_delay_slot, (void)core;
#endif
    return true;
}
