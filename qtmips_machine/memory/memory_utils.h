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

#include <cstdint>
#include <cstdlib>
#include <cstring>

namespace machine {

/**
 * Additional options for read operation between memory layers
 *
 *  The puprose for this struct is to make the API easily
 *   extensible.
 */
struct ReadOptions {
    bool debug = false;
};

/**
 * Additional options for write operation between memory layers
 *
 *  The puprose for this struct is to make the API easily
 *   extensible.
 */
struct WriteOptions {
    // Reserved for future use
};

struct ReadResult {
    /**
     * Number of bytes successfully read.
     *
     * May be lower than requested size in case partial success
     *  like page fault.
     */
    size_t n_bytes;

    inline ReadResult operator+(const ReadResult& other)
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

    inline WriteResult operator+(const WriteResult& other)
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
    case 8:
        *(uint64_t*)dst = *(uint64_t*)src;
        break;
    case 4:
        *(uint32_t*)dst = *(uint32_t*)src;
        break;
    case 2:
        *(uint16_t*)dst = *(uint16_t*)src;
        break;
    case 1:
        *(uint8_t*)dst = *(uint8_t*)src;
        break;
    default:
        memcpy(dst, src, size);
    }
}

/**
 * Generic memset function optimized for small sizes.
 *
 *  Sizes 1-8 bytes are performed by assignment.
 *  Other sizes fallback to libc memset.
 *
 * @param dst       destination data pointer
 * @param val       value to set, low bits for smaller types, repeat for larger types
 * @param size      number of bytes to set
 */
inline void memory_set(void* dst, int val, size_t size)
{
    switch (size) {
    case 8:
        *(uint64_t*)dst = (uint64_t)val;
        break;
    case 4:
        *(uint32_t*)dst = (uint32_t)val;
        break;
    case 2:
        *(uint16_t*)dst = (uint16_t)val;
        break;
    case 1:
        *(uint8_t*)dst = (uint8_t)val;
        break;
    default:
        memset(dst, val, size);
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
    case 8:
        return *(uint64_t*)first == *(uint64_t*)second;
    case 4:
        return *(uint32_t*)first = *(uint32_t*)second;
    case 2:
        return *(uint16_t*)first = *(uint16_t*)second;
    case 1:
        return *(uint8_t*)first = *(uint8_t*)second;
    default:
        return memcmp(first, second, size) == 0;
    }
}

/**
 * Combination of `memory_compare` and `memory_copy`

 * @param dst       destination data pointer
 * @param src       source data pointer
 * @param size      number of bytes to copy
 * @param offset    small offset to destination pointer to simplify write to part of variable
 * @return          true if data is identical
 */
inline bool memory_compare_and_copy(void* _dst, const void* src, size_t size, size_t dst_offset = 0)
{
    // This simplifies writing to the middle of variable a lot and hides all the casting of void*
    byte* dst = (byte*)_dst + dst_offset;

    if (memory_compare(dst, src, size)) {
        return true;
    }

    memory_copy(dst, src, size);
    return false;
}

}

#endif //QTMIPS_MEMORY_UTILS_H
