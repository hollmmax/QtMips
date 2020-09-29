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

#ifndef CACHE_H
#define CACHE_H

#include "../machineconfig.h"
#include "frontend_memory.h"
#include "cache_types.h"
#include "cache_policy.h"

#include <cstdint>
#include <ctime>

namespace machine {

constexpr size_t BLOCK_SIZE = sizeof(uint32_t);

/**
 * Terminology:
 *  - Cache contains a table in `dt` for each degree of associativity.
 *  - Each table has `rows` a.k.a. `sets`.
 *  - Each sets consist of blocks of type `uint32_t`. Size of block
 *    is only implementation detail, important is size of a set.
 */
class Cache : public FrontendMemory {

    Q_OBJECT

public:
    Cache(
        FrontendMemory *m,
        const CacheConfig *c,
        unsigned memory_access_penalty_r = 1,
        unsigned memory_access_penalty_w = 1,
        unsigned memory_access_penalty_b = 0
    );

    ~Cache() override;

    bool write(
        Address destination,
        const void *source,
        size_t size
    ) override;

    void read(
        Address source,
        void *destination,
        size_t size,
        bool debug_access
    ) const override;

    std::uint32_t get_change_counter() const override;

    void flush();         // flush cache
    void sync() override; // Same as flush

    unsigned get_hit_count() const;   // Number of recorded hits
    unsigned get_miss_count() const;  // Number of recorded misses
    unsigned get_read_count() const;  // Number backing/main memory reads
    unsigned get_write_count() const; // Number backing/main memory writes
    unsigned get_stall_count()
        const; // Number of wasted get_cycle_count in memory waiting statistic
    double get_speed_improvement()
        const;                   // Speed improvement in percents in comare with no used cache
    double get_hit_rate() const; // Usage efficiency in percents

    void reset(); // Reset whole state of cache

    const CacheConfig &get_config() const;

    enum LocationStatus location_status(Address address) const override;

signals:

    void hit_update(unsigned) const;

    void miss_update(unsigned) const;

    void statistics_update(
        unsigned stalled_cycles,
        double speed_improv,
        double hit_rate) const;

    void cache_update(
        unsigned associat,
        unsigned set,
        unsigned col,
        bool valid,
        bool dirty,
        std::uint32_t tag,
        const std::uint32_t *data,
        bool write) const;

    void memory_writes_update(unsigned) const;

    void memory_reads_update(unsigned) const;

private:
    CacheConfig cache_config;
    FrontendMemory *mem = nullptr;
    unsigned access_pen_r, access_pen_w, access_pen_b;
    Address uncached_start = 0x0_addr;
    Address uncached_last = 0x0_addr;

    mutable struct cache_data **dt;

    CachePolicy* replacement_policy;

    mutable unsigned hit_read, miss_read, hit_write,
        miss_write = 0; // Hit and miss counters
    mutable unsigned mem_reads, mem_writes, burst_reads,
        burst_writes; // Dirrect access to memory
    mutable std::uint32_t change_counter;

    std::uint32_t debug_rword(Address address) const;

    bool access(
        Address address,
        void *buffer,
        bool write,
        size_t size
    ) const;

    void kick(unsigned associativity_index, unsigned row) const;

    Address base_address(std::uint32_t tag, unsigned row) const;

    void update_statistics() const;

    constexpr inline CacheLocation
    compute_location(const Address address) const;

    size_t search_cache_line(const CacheLocation &loc) const;

};

} // namespace machine

#endif // CACHE_H
