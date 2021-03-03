

#ifndef MEMORY_MMAP_H
#define MEMORY_MMAP_H

#include "memory/backend/memory.h"

namespace machine {

class MemoryMmap final : public BackendMemory {
public:
    ~MemoryMmap() override;
    explicit MemoryMmap(Endian simulated_machine_endian);

public:
    WriteResult write(
        Offset destination,
        const void *source,
        size_t size,
        WriteOptions options) override;

    ReadResult read(
        void *destination,
        Offset source,
        size_t size,
        ReadOptions options) const override;

    LocationStatus location_status(Offset offset) const override;

private:
    byte *const storage;
    static byte *allocate_storage();
};

} // namespace machine

#endif // MEMORY_MMAP_H
