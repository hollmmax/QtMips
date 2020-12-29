// SPDX-License-Identifier: GPL-2.0+
/*******************************************************************************
 * QtMips - MIPS 32-bit Architecture Subset Simulator
 *
 * Implemented to support following courses:
 *
 *   B35APO - Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b35apo
 *
 *   B4M35PAP - Advanced Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b4m35pap/start
 *
 * Copyright (c) 2017-2019 Karel Koci<cynerd@email.cz>
 * Copyright (c) 2019      Pavel Pisa <pisa@cmp.felk.cvut.cz>
 * Copyright (c) 2020      Jakub Dupak <dupak.jakub@gmail.com>
 * Copyright (c) 2020      Max Hollmann <hollmmax@fel.cvut.cz>
 *
 * Faculty of Electrical Engineering (http://www.fel.cvut.cz)
 * Czech Technical University        (http://www.cvut.cz/)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301, USA.
 *
 ******************************************************************************/

#ifndef QTMIPS_MEMORY_UTILS_H
#define QTMIPS_MEMORY_UTILS_H

#include "../utils.h"
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <functional>

namespace machine {

enum Endianness { LITTLE, BIG };

/**
 * Additional options for read operation between memory layers
 *
 *  The puprose for this struct is to make the API easily
 *   extensible.
 */
struct ReadOptions {
    bool debug;
    //    Endianness cpu_endianness;
};

/**
 * Additional options for write operation between memory layers
 *
 *  The puprose for this struct is to make the API easily
 *   extensible.
 */
struct WriteOptions {
    //    Endianness cpu_endianness;
};

struct ReadResult {
    /**
     * Number of bytes successfully read.
     *
     * May be lower than requested size in case partial success
     *  like page fault.
     */
    size_t n_bytes = 0;

    inline ReadResult operator+(const ReadResult& other) const
    {
        return {
            this->n_bytes + other.n_bytes,
        };
    }

    inline void operator+=(const ReadResult& other)
    {
        this->n_bytes += other.n_bytes;
    }
};

struct WriteResult {
    /**
     * Number of bytes successfully read.
     *
     * May be lower than requested size in case partial success
     *  like page fault.
     */
    size_t n_bytes;

    /**
     * Indicates whether write caused change in memory.
     * Used to reduce UI redraws.
     */
    bool changed;

    inline WriteResult operator+(const WriteResult& other) const
    {
        return {
            this->n_bytes + other.n_bytes,
            this->changed || other.changed,
        };
    }

    inline void operator+=(const WriteResult& other)
    {
        this->n_bytes += other.n_bytes;
        this->changed |= other.changed;
    }
};

/**
 * Notification to UI about a memory change.
 *
 * Tagged union tagged by `size` attribute. Values that can fit into uint64_t
 * will be sent by value, otherwise a pointer to storing area will be sent. If
 * storing area is volatile, sender has to split notification into multiple
 * calls.
 * // TODO endianness with a pointer access.
 * // TODO is this necessary?
 */
struct AccessNotification {
    size_t Offset;
    size_t size;
    union {
        const void* pointer;
        uint64_t u64;
        uint32_t u32;
        uint16_t u16;
        uint8_t u8;
    } value;

    AccessNotification(size_t offset, uint64_t value)
        : Offset(offset)
        , size(sizeof(uint64_t))
    {
        this->value.u64 = value;
    }
};

/**
 * Generic memcpy function optimized for small copies.
 *
 *  Sizes 1-8 bytes are performed by assignment.
 *  Other sizes fallback to libc memcpy.
 *
 * @param dst       destination data pointer
 * @param src       source data pointer
 * @param size      number of bytes to copy
 */
inline void memory_copy(void* dst, const void* src, size_t size)
{
    switch (size) {
    case 8: *(uint64_t*)dst = *(uint64_t*)src; break;
    case 4: *(uint32_t*)dst = *(uint32_t*)src; break;
    case 3:
        *(uint32_t*)dst = *(uint32_t*)src;
        *((uint8_t*)dst + 4) = *((uint8_t*)src + 4);
        break;
    case 2: *(uint16_t*)dst = *(uint16_t*)src; break;
    case 1: *(uint8_t*)dst = *(uint8_t*)src; break;
    default: memcpy(dst, src, size);
    }
}

/**
 * Generic memset function optimized for small sizes.
 *
 *  Sizes 1-8 bytes are performed by assignment.
 *  Other sizes fallback to libc memset.
 *
 * @param dst       destination data pointer
 * @param val       value to set, low bits for smaller types, repeat for larger
 * types
 * @param size      number of bytes to set
 */
inline void memory_set(void* dst, int val, size_t size)
{
    switch (size) {
    case 8: *(uint64_t*)dst = (uint64_t)val; break;
    case 4: *(uint32_t*)dst = (uint32_t)val; break;
    case 2: *(uint16_t*)dst = (uint16_t)val; break;
    case 1: *(uint8_t*)dst = (uint8_t)val; break;
    default: memset(dst, val, size);
    }
}

/**
 * Generic memcmp function optimized for small copies.
 *
 *  Sizes 1-8 bytes are compared trivially.
 *  Other sizes fallback to libc memcmp.
 *
 * @param first     pointer to data to be compared
 * @param second    pointer to data to be compared
 * @param size      number of bytes to compare
 * @return          true if data is identical
 */
inline bool memory_compare(const void* first, const void* second, size_t size)
{
    switch (size) {
    case 8: return *(uint64_t*)first == *(uint64_t*)second;
    case 4: return *(uint32_t*)first == *(uint32_t*)second;
    case 2: return *(uint16_t*)first == *(uint16_t*)second;
    case 1: return *(uint8_t*)first == *(uint8_t*)second;
    default: return memcmp(first, second, size) == 0;
    }
}

// TODO Conside optimizing by compiler intrinsics, especially Windows
inline uint32_t byte_swap_32(uint32_t a)
{
    uint32_t b;
    byte* bp = reinterpret_cast<byte*>(&b);
    byte* ap = reinterpret_cast<byte*>(&a);
    bp[0] = ap[3];
    bp[1] = ap[3];
    bp[2] = ap[1];
    bp[3] = ap[0];
    return b;
}

/**
 * Byte swap wrapper that swaps only on when supplied endiannesses do not match.
 *  Otherwise a identity function.
 */
inline uint32_t convert_endianness_32(uint32_t a, Endianness from, Endianness to)
{
    return (from == to) ? a : byte_swap_32(a);
}

/**
 * When converting n-byte memory access into alligned series of discreete
 *  accesses each by the STORAGE_TYPE, this function returns size of useful
 *  data in next access.
 *
 * Example:
 * Periphery suports write by uint32_t. Access of size 4 targets in the middle
 *  of a uint32_t register. Then this funnction will return 2, which means that
 *  last 2 bytes of the retrieved data will be used (written to register).
 *
 * @tparam STORAGE_TYPE     a type peripery supports for access
 * @param ptr               pointer-like value used for access
 * @return                  size to be used from alligned access
 */
template <typename STORAGE_TYPE>
inline size_t partial_access_size(uintptr_t ptr)
{
    size_t size = ptr % sizeof(STORAGE_TYPE);
    return (size == 0) ? sizeof(STORAGE_TYPE) : size;
}

/**
 * Perform n-byte read into periphery that only supports u32 access.
 *
 * @tparam FUNC             function :: size_t -> uint32_t
 * @param src               data offset in periphery
 * @param dst               pointer to write to
 * @param size              n bytes
 * @param data_getter       function object which return u32 data for given
 */
template <typename FUNC>
inline ReadResult
read_by_u32(size_t src, void* dst, size_t size, FUNC data_getter)
{
    size_t current_src = src;
    byte* current_dst = static_cast<byte*>(dst);
    size_t remaining_size = size;

    do {
        size_t partial_size = partial_access_size<uint32_t>(current_src);

        uint32_t data = data_getter(current_src & ~3u);

        size_t data_offset = sizeof(uint32_t) - partial_size;
        memory_copy(current_dst, (byte*)&data + data_offset, partial_size);

        remaining_size -= partial_size;
        current_src += partial_size;
        current_dst += partial_size;
    } while (remaining_size > 0);

    return { .n_bytes = size };
}

/**
 * Perform n-byte write into periphery that only supports u32 access.
 *
 * @tparam FUNC1            function :: size_t -> uint32_t
 * @tparam FUNC2            function :: size_t, uint32_t -> bool
 * @param src               data source
 * @param dst               offset in periphery
 * @param size              n bytes
 * @param data_getter       function object which return u32 data for given
 *                           offset
 * @param data_setter       function object which writes an u32 to givem offset
 * @return                  true if write caused a change
 */
template <typename FUNC1, typename FUNC2>
inline WriteResult write_by_u32(
    const void* src,
    size_t dst,
    size_t size,
    FUNC1 data_getter,
    FUNC2 data_setter)
{
    const byte* current_src = static_cast<const byte*>(src);
    size_t current_dst = dst;
    size_t remaining_size = size;
    bool changed = false;

    do {
        size_t partial_size = partial_access_size<uint32_t>(current_dst);
        uint32_t data = data_getter(current_dst & ~3u);
        size_t data_offset = sizeof(uint32_t) - partial_size;
        memory_copy((byte*)&data + data_offset, current_src, partial_size);

        changed |= data_setter(current_dst & ~3u, data);

        remaining_size -= partial_size;
        current_src += partial_size;
        current_dst += partial_size;
    } while (remaining_size > 0);

    return { .n_bytes = size, .changed = changed };
}

/**
 * In case that underlying memory representation is fragmented, multiple
 * invocations of the same code might be needed. This is a common case with the
 * n-byte memory access API and therefore this function has been introduce to
 * minimize code duplication.
 *
 * @tparam FUNC         function with same signature as read or write
 * @tparam SRC_TYPE     corresponding type in read or write
 * @tparam DST_TYPE     corresponding type in read or write
 * @tparam OPTIONS_TYPE ReadOptions or WriteOptions
 * @tparam RESULT_TYPE  ReadResult or WriteResult
 * @param src           same arg as in read/write
 * @param dst           same arg as in read/write
 * @param size          same arg as in read/write
 * @param options       same arg as in read/write
 * @param function      lambda to perform individual accesses
 * @return number of bytes obtained, == size if fully successful
 */
template<
    typename RESULT_TYPE,
    typename FUNC,
    typename SRC_TYPE,
    typename DST_TYPE,
    typename OPTIONS_TYPE>
inline RESULT_TYPE repeat_access_until_completed(
    DST_TYPE dst,
    SRC_TYPE src,
    size_t size,
    OPTIONS_TYPE options,
    FUNC function) {
    size_t remaining_size = size;
    const byte* current_src = reinterpret_cast<const byte*>(src);
    byte* current_dst = reinterpret_cast<byte*>(dst);
    RESULT_TYPE total_result;

    // do-while is preffered, because this loop is most likely to be executed
    // only once
    do {
        RESULT_TYPE result = function(
            reinterpret_cast<DST_TYPE>(current_dst),
            reinterpret_cast<SRC_TYPE>(current_src), remaining_size, options);
        total_result += result;
        current_src += result.n_bytes;
        current_dst += result.n_bytes;
        remaining_size -= result.n_bytes;
    } while (remaining_size > 0);

    return total_result;
}

/**
 * Helper function for memories, that do not support function like read_u32.
 * It is used in tests.
 * This is a generic version followed by named instantiations.
 */
template<typename T, typename MEM_T, typename ADDR_T>
T memory_read(MEM_T* mem, ADDR_T address) {
    T buffer;
    mem->read(&buffer, address, sizeof(T), {});
    return byteswap(buffer);
}

template<typename MEM_T, typename ADDR_T>
uint8_t memory_read_u8(MEM_T* mem, ADDR_T address) {
    return memory_read<uint8_t>(mem, address);
}

template<typename MEM_T, typename ADDR_T>
uint16_t memory_read_u16(MEM_T* mem, ADDR_T address) {
    return memory_read<uint16_t>(mem, address);
}

template<typename MEM_T, typename ADDR_T>
uint32_t memory_read_u32(MEM_T* mem, ADDR_T address) {
    return memory_read<uint32_t>(mem, address);
}

template<typename MEM_T, typename ADDR_T>
uint64_t memory_read_u64(MEM_T* mem, ADDR_T address) {
    return memory_read<uint64_t>(mem, address);
}

/**
 * Helper function for memories, that do not support function like read_u32.
 * It is used in tests.
 * This is a generic version followed by named instantiations.
 */
template<typename T, typename MEM_T, typename ADDR_T>
void memory_write(MEM_T* mem, ADDR_T address, T value) {
    const T swapped_value = byteswap(value);
    mem->write(address, &swapped_value, sizeof(T), {});
}

#endif // QTMIPS_MEMORY_UTILS_H
