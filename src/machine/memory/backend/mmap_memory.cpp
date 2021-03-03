#include "mmap_memory.h"

#include <sys/mman.h>

namespace machine {
constexpr size_t SIZE = 0xFFFFFFFF;

MemoryMmap::MemoryMmap(machine::Endian simulated_machine_endian)
    : BackendMemory(simulated_machine_endian)
    , storage(allocate_storage()) {}

byte *machine::MemoryMmap::allocate_storage() {
    void *res = mmap(
        nullptr, SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1,
        0);
    if (res == MAP_FAILED) {
        perror("MemoryMmap::mmap creation failed");
        throw SimulatorExceptionRuntime("Mmap failed", "", __FILE__, __LINE__);
    }
    return static_cast<byte *>(res);
}

WriteResult machine::MemoryMmap::write(
    machine::Offset destination,
    const void *source,
    size_t size,
    machine::WriteOptions options) {
    UNUSED(options)
    Q_ASSERT(destination + size <= SIZE);
    bool changed = memcmp(&storage[destination], source, size) != 0;
    memcpy(&storage[destination], source, size);
    return { .n_bytes = size, .changed = changed };
}

ReadResult machine::MemoryMmap::read(
    void *destination,
    machine::Offset source,
    size_t size,
    machine::ReadOptions options) const {
    UNUSED(options)
    Q_ASSERT(source + size <= SIZE);
    memcpy(destination, &storage[source], size);
    return { .n_bytes = size };
}

LocationStatus MemoryMmap::location_status(Offset offset) const {
    if (offset <= SIZE) {
        return LOCSTAT_NONE;
    } else {
        return LOCSTAT_ILLEGAL;
    }
}

MemoryMmap::~MemoryMmap() {
    munmap(storage, SIZE);
}

} // namespace machine
