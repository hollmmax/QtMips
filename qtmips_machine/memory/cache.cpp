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

#include <cstdlib>
#include "cache.h"
#include "memory_utils.h"
#include "cache_types.h"

using namespace machine;

Cache::Cache(
    FrontendMemory *m,
    const CacheConfig *cc,
    unsigned memory_access_penalty_r,
    unsigned memory_access_penalty_w,
    unsigned memory_access_penalty_b
)
    : cache_config(cc),
    mem(m),
    access_pen_b(memory_access_penalty_b), {
    mem = m;
    access_pen_r = memory_access_penalty_r;
    access_pen_w = memory_access_penalty_w;
    access_pen_b = memory_access_penalty_b;
    uncached_start = 0xf0000000_addr;
    uncached_last = 0xffffffff_addr;
    // Zero hit and miss rate
    hit_read = 0;
    hit_write = 0;
    miss_read = 0;
    miss_write = 0;
    mem_reads = 0;
    mem_writes = 0;
    burst_reads = 0;
    burst_writes = 0;
    dt = nullptr;
    replacement_policy = nullptr;
    change_counter = 0;

    // Skip any other initialization if cache is disabled
    if (!cc->enabled())
        return;

    // Allocate cache data structure
    dt = new struct cache_data *[cc->associativity()];
    for (unsigned i = 0; i < cc->associativity(); i++) {
        dt[i] = new cache_data[cc->set_count()];
        for (unsigned y = 0; y < cc->set_count(); y++) {
            dt[i][y].valid = false;
            dt[i][y].dirty = false;
            dt[i][y].data = new uint32_t[cc->block_count()];
        }
    }

    replacement_policy = CachePolicy::get_policy(cc, dt);
}

Cache::~Cache() {
    if (dt != nullptr) {
        for (unsigned i = 0; i < cache_config.associativity(); i++) {
            if (dt[i]) {
                for (unsigned y = 0; y < cache_config.set_count(); y++)
                    delete[] dt[i][y].data;
                delete[] dt[i];
            }
        }
        delete[] dt;
    }
    delete replacement_policy;
}


bool Cache::write(
    Address address,
    const void *source,
    size_t size
) {
    bool changed;

    if (!cache_config.enabled() || (address >= uncached_start && address <= uncached_last)) {
        mem_writes++;
        emit memory_writes_update(mem_writes);
        update_statistics();
        return mem->write(address, source, size);
    }

    changed = access(address, &source, true, size);

    if (cache_config.write_policy() != CacheConfig::WP_BACK) {
        mem_writes++;
        emit memory_writes_update(mem_writes);
        update_statistics();
        return mem->write(address, source, size);
    }
    return changed;
}

void Cache::read(
    Address source,
    void *destination,
    size_t size,
    bool debug_access
) const {
    if (!cache_config.enabled() || (source >= uncached_start && source <= uncached_last)) {
        mem_reads++;
        emit memory_reads_update(mem_reads);
        update_statistics();
        return mem->read(source, destination, size, debug_access);
    }

    if (debug_access) {
        if (!(location_status(source) & LOCSTAT_CACHED))
            return mem->read(source, destination, size, debug_access);
        return debug_rword(source);
    }

    access(source, &destination, false, size);
}

std::uint32_t Cache::get_change_counter() const {
    return change_counter;
}

void Cache::flush() {
    if (!cache_config.enabled())
        return;

    for (unsigned as = cache_config.associativity(); as-- > 0;)
        for (unsigned st = 0; st < cache_config.set_count(); st++)
            if (dt[as][st].valid) {
                kick(as, st);
                emit cache_update(as, st, 0, false, false, 0, 0, false);
            }
    change_counter++;
    update_statistics();
}

void Cache::sync() {
    flush();
}

unsigned Cache::get_hit_count() const {
    return hit_read + hit_write;
}

unsigned Cache::get_miss_count() const {
    return miss_read + miss_write;
}

unsigned Cache::get_read_count() const {
    return mem_reads;
}

unsigned Cache::get_write_count() const {
    return mem_writes;
}

unsigned Cache::get_stall_count() const {
    unsigned st_cycles = mem_reads * (access_pen_r - 1) + mem_writes * (access_pen_w - 1);
    if (access_pen_b != 0)
        st_cycles -= burst_reads * (access_pen_r - access_pen_b) +
                     burst_writes * (access_pen_w - access_pen_b);
    return st_cycles;
}

double Cache::get_speed_improvement() const {
    unsigned lookup_time;
    unsigned mem_access_time;
    unsigned comp = hit_read + hit_write + miss_read + miss_write;
    if (comp == 0)
        return 100.0;
    lookup_time = hit_read + miss_read;
    if (cache_config.write_policy() == CacheConfig::WP_BACK)
        lookup_time += hit_write + miss_write;
    mem_access_time = mem_reads * access_pen_r + mem_writes * access_pen_w;
    if (access_pen_b != 0)
        mem_access_time -= burst_reads * (access_pen_r - access_pen_b) +
                           burst_writes * (access_pen_w - access_pen_b);
    return (double) ((miss_read + hit_read) * access_pen_r + (miss_write + hit_write) * access_pen_w) \
 / (double) (lookup_time + mem_access_time) \
 * 100;
}

double Cache::get_hit_rate() const {
    unsigned comp = hit_read + hit_write + miss_read + miss_write;
    if (comp == 0)
        return 0.0;
    return (double) (hit_read + hit_write) / (double) comp * 100.0;
}

void Cache::reset() {
    // Set all cells to invalid
    if (cache_config.enabled()) {
        for (unsigned as = 0; as < cache_config.associativity(); as++)
            for (unsigned st = 0; st < cache_config.set_count(); st++)
                dt[as][st].valid = false;
    }
    // Note: we don't have to zero replacement policy data as those are zeroed when first used on invalid cell
    // Zero hit and miss rate
    hit_read = 0;
    hit_write = 0;
    miss_read = 0;
    miss_write = 0;
    mem_reads = 0;
    mem_writes = 0;
    burst_reads = 0;
    burst_writes = 0;
    // Trigger signals
    emit hit_update(get_hit_count());
    emit miss_update(get_miss_count());
    emit memory_reads_update(get_read_count());
    emit memory_writes_update(get_write_count());
    update_statistics();
    if (cache_config.enabled()) {
        for (unsigned as = 0; as < cache_config.associativity(); as++)
            for (unsigned st = 0; st < cache_config.set_count(); st++)
                emit cache_update(as, st, 0, false, false, 0, 0, false);
    }
}

const CacheConfig &Cache::get_config() const {
    return cache_config;
}

enum LocationStatus Cache::location_status(Address address) const {
    CacheLocation loc = compute_location(address);

    if (cache_config.enabled()) {
        for (unsigned indx = 0; indx < cache_config.associativity(); indx++) {
            if (dt[indx][loc.row].valid && dt[indx][loc.row].tag == loc.tag) {
                if (dt[indx][loc.row].dirty &&
                    cache_config.write_policy() == CacheConfig::WP_BACK)
                    return (enum LocationStatus) (LOCSTAT_CACHED | LOCSTAT_DIRTY);
                else
                    return (enum LocationStatus) LOCSTAT_CACHED;
            }
        }
    }
    return mem->location_status(address);
}

std::uint32_t Cache::debug_rword(Address address) const {
    CacheLocation loc = compute_location(address);
    for (unsigned index = 0; index < cache_config.associativity(); index++)
        if (dt[index][loc.row].valid && dt[index][loc.row].tag == loc.tag)
            return dt[index][loc.row].data[loc.col];
    return 0;
}

bool Cache::access(
    Address address,
    void *buffer,
    bool write,
    size_t size
) const {
    bool changed = false;
    CacheLocation loc = compute_location(address);

    size_t index = search_cache_line(loc);
    // Need to find new block
    if (index >= cache_config.associativity()) {
        // if write through we do not need to alloecate cache line does not allocate
        if (write && cache_config.write_policy() == CacheConfig::WP_THROUGH_NOALLOC) {
            miss_write++;
            emit miss_update(get_miss_count());
            update_statistics();
            return false;
        }
        // We have to kick something
        index = replacement_policy->select_index_to_evict(loc);
    }
    SANITY_ASSERT(index < cache_config.associativity(), "Probably unimplemented replacement policy");

    struct cache_data &cd = dt[index][loc.row];

    // Verify if we are not replacing
    if (cd.valid && cd.tag != loc.tag) {
        kick(index, loc.row);
        change_counter++;
    }

    // Update statistics and otherwise read from memory
    if (cd.valid) {
        if (write) {
            hit_write++;
        }
        else {
            hit_read++;
        }
        emit hit_update(get_hit_count());
        update_statistics();
    } else {
        if (write) {
            miss_write++;
        } else {
            miss_read++;
        }
        emit miss_update(get_miss_count());
        mem->read(base_address(loc.tag, loc.row), cd.data, cache_config.block_count() * BLOCK_SIZE, false);
        change_counter += cache_config.block_count();
        mem_reads += cache_config.block_count();
        burst_reads += cache_config.block_count() - 1;
        emit memory_reads_update(mem_reads);
        update_statistics();
    }

    // Update replcement data
    replacement_policy->update_replacement_data(index, loc, cd.valid);

    cd.valid = true; // We either write to it or we read from memory. Either way it's valid when we leave Cache class
    cd.dirty = cd.dirty || write;
    cd.tag = loc.tag;


    int64_t overlap = loc.col * BLOCK_SIZE
                      + size - cache_config.block_count() * BLOCK_SIZE;
    overlap = std::max(overlap, {0});

    size_t size1 = size - overlap;

    if (write) {
        changed = !memory_compare(&cd.data[loc.col] + loc.subblock, buffer, size1);
        if (changed) {
            memory_copy(&cd.data[loc.col] + loc.subblock, buffer, size1);
        }
    } else {
        /* Read */
        memory_copy(buffer, &cd.data[loc.col] + loc.subblock, size1);
    }

    uint32_t last_affected_col = (loc.col * BLOCK_SIZE + size1) / BLOCK_SIZE;
    while (loc.col <= last_affected_col) {
        emit cache_update(index, loc.row, loc.col, cd.valid, cd.dirty, cd.tag, cd.data, write);
        loc.col += 1;
    }

    if (changed) {
        change_counter++;
    }

    if (overlap > 0) {
        /*
         * If access overlaps single cache row, perform access to next
         *  row recursively.
         */
        changed |= access(
            address + (size1 * 8),
            (uint8_t *) buffer + (size1 * 8),
            write, overlap
        );
    }

    return changed;
}

size_t Cache::search_cache_line(const CacheLocation &loc) const {
    unsigned index = 0;
    // Try to locate exact block
    while (index < cache_config.associativity() && (!dt[index][loc.row].valid || dt[index][loc.row].tag != loc.tag))
        index++;
    return index;
}

void Cache::kick(
    unsigned associativity_index,
    unsigned row
) const {
    struct cache_data &cd = dt[associativity_index][row];
    if (cd.dirty && cache_config.write_policy() == CacheConfig::WP_BACK) {
        for (unsigned i = 0; i < cache_config.block_count(); i++)
            mem->write_word(base_address(cd.tag, row) + (4 * i), cd.data[i]);
        mem_writes += cache_config.block_count();
        burst_writes += cache_config.block_count() - 1;
        emit memory_writes_update(mem_writes);
    }
    cd.valid = false;
    cd.dirty = false;

    // TODO move to repl policy
    switch (cache_config.replacement_policy()) {
        case CacheConfig::RP_LRU: {
            unsigned next_asi = associativity_index;
            unsigned tmp_asi = replc.lru[row][0];
            int i = 1;
            while (tmp_asi != associativity_index) {
                SANITY_ASSERT(i < (int) cache_config.associativity(), "LRU lost the way from priority queue - kick");
                tmp_asi = replc.lru[row][i];
                replc.lru[row][i] = next_asi;
                next_asi = tmp_asi;
                i++;
            }
            break;
        }
        case CacheConfig::RP_LFU:
            replc.lfu[row][associativity_index] = 0;
            break;
        default:
            break;
    }
}


void Cache::update_statistics() const {
    emit statistics_update(get_stall_count(), get_speed_improvement(), get_hit_rate());
}

Address Cache::base_address(
    std::uint32_t tag,
    unsigned row
) const {
    return Address(
        (
            (tag * cache_config.block_count() * cache_config.set_count())
            + (row * cache_config.block_count())
        ) << 2U);
}


constexpr CacheLocation Cache::compute_location(Address address) const {
    uint32_t block_index = address.get_raw() / BLOCK_SIZE;
    uint32_t block_count = cache_config.block_count() * cache_config.set_count();
    uint32_t index = block_index % block_count;
    return {
        .row = index / cache_config.block_count(),
        .col = index % cache_config.block_count(),
        .tag = block_index / block_count,
        .subblock = static_cast<uint32_t>(address.get_raw() % BLOCK_SIZE)
    };
}

