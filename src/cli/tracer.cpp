#include "tracer.h"

#include <cinttypes>

using namespace machine;

Tracer::Tracer(Machine *machine) : core_state(machine->core()->state) {
    connect(machine->core(), &Core::step_done, this, &Tracer::step_output);
}

void Tracer::step_output() {
    const auto &fetch = core_state.pipeline.fetch.final;
    const auto &decode = core_state.pipeline.decode.final;
    const auto &exec = core_state.pipeline.execute.final;
    const auto &mem = core_state.pipeline.memory.final;
    const auto &wb = core_state.pipeline.writeback.internal;
    if (trace_fetch) {
        auto exception_mark = (fetch.excause != EXCAUSE_NONE ? "!" : "");
        printf(
            "Fetch: %s%s\n", exception_mark,
            qPrintable(wb.inst.to_str(fetch.inst_addr)));
    }
    if (trace_decode) {
        auto exception_mark = (decode.excause != EXCAUSE_NONE ? "!" : "");
        printf(
            "Decode: %s%s\n", exception_mark,
            qPrintable(wb.inst.to_str(decode.inst_addr)));
    }
    if (trace_execute) {
        auto exception_mark = (exec.excause != EXCAUSE_NONE ? "!" : "");
        printf(
            "Execute: %s%s\n", exception_mark,
            qPrintable(wb.inst.to_str(exec.inst_addr)));
    }
    if (trace_memory) {
        auto exception_mark = (mem.excause != EXCAUSE_NONE ? "!" : "");
        printf(
            "Memory: %s%s\n", exception_mark,
            qPrintable(wb.inst.to_str(mem.inst_addr)));
    }
    if (trace_writeback) {
        printf("Writeback: %s\n", qPrintable(wb.inst.to_str(wb.inst_addr)));
    }
    if (trace_pc) {
        printf("PC: %" PRIx64 "\n", fetch.inst_addr.get_raw());
    }
    if (trace_regs_gp && wb.regwrite && regs_to_trace.at(wb.num_rd)) {
        printf("GP %d: %" PRIx64 "\n", wb.num_rd, wb.to_write_val.as_u64());
    }
}