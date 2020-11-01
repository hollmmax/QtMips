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

#include "cache.h"
#include "cache_types.h"
#include <cstdlib>

namespace machine {

Cache::Cache(
    FrontendMemory* memory,
    const CacheConfig* config,
    uint32_t memory_access_penalty_r,
    uint32_t memory_access_penalty_w,
    uint32_t memory_access_penalty_b)
    : cache_config(config)
    , mem(memory)
    , uncached_start(0xf0000000_addr)
    , uncached_last(0xfffffffe_addr)
    , access_pen_r(memory_access_penalty_r)
    , access_pen_w(memory_access_penalty_w)
    , access_pen_b(memory_access_penalty_b)
    , replacement_policy(CachePolicy::get_policy_instance(config))
{

    // Skip memory allocation if cache is disabled
    if (!config->enabled()) {
        return;
    }

    dt.resize(
        config->associativity(),
        std::vector<CacheBlock>(
            config->set_count(),
            { .valid = false,
              .dirty = false,
              .tag = 0,
              .data = std::vector<uint32_t>(config->block_size()) }));
}

Cache::~Cache() = default;

WriteResult Cache::write(Address address, const void* source, size_t size)
{
    if (!cache_config.enabled()
        || (address >= uncached_start && address <= uncached_last)) {
        mem_writes++;
        emit memory_writes_update(mem_writes);
        update_all_statistics();
        return mem->write(address, source, size);
    }

    const bool changed = access(address, &source, size, WRITE);

    if (cache_config.write_policy() != CacheConfig::WP_BACK) {
        mem_writes++;
        emit memory_writes_update(mem_writes);
        update_all_statistics();
        return mem->write(address, source, size);
    }

    return { .n_bytes = size, .changed = changed };
}

ReadResult Cache::read(
    Address source,
    void* destination,
    size_t size,
    ReadOptions options) const
{
    if (!cache_config.enabled()
        || (source >= uncached_start && source <= uncached_last)) {
        mem_reads++;
        emit memory_reads_update(mem_reads);
        update_all_statistics();
        return mem->read(source, destination, size, options);
    }

    if (options.debug) {
        if (!(location_status(source) & LOCSTAT_CACHED)) {
            mem->read(source, destination, size, options);
        } else {
            debug_rword(source, destination, size);
        }
        return {};
    }

    access(source, &destination, size, READ);

    return {};
}

void Cache::flush()
{
    if (!cache_config.enabled()) {
        return;
    }

    for (size_t assoc_index = cache_config.associativity(); assoc_index > 0;
         assoc_index--) {
        for (size_t set_index = 0; set_index < cache_config.set_count();
             set_index++) {
            if (dt[assoc_index][set_index].valid) {
                kick(assoc_index, set_index);
                emit cache_update(
                    assoc_index, set_index, 0, false, false, 0, nullptr, false);
            }
        }
    }
    change_counter++;
    update_all_statistics();
}

void Cache::sync() { flush(); }

void Cache::reset()
{
    // Set all cells to invalid
    if (cache_config.enabled()) {
        for (auto& set : dt) {
            for (auto& block : set) {
                block.valid = false;
            }
        }
    }

    // Note: we don't have to zero replacement policy data as those are zeroed
    // when first used on invalid cell

    hit_read = 0;
    hit_write = 0;
    miss_read = 0;
    miss_write = 0;
    mem_reads = 0;
    mem_writes = 0;
    burst_reads = 0;
    burst_writes = 0;

    emit hit_update(get_hit_count());
    emit miss_update(get_miss_count());
    emit memory_reads_update(get_read_count());
    emit memory_writes_update(get_write_count());
    update_all_statistics();

    if (cache_config.enabled()) {
        for (size_t assoc_index = 0; assoc_index < cache_config.associativity();
             assoc_index++) {
            for (size_t set_index = 0; set_index < cache_config.set_count();
                 set_index++) {
                emit cache_update(
                    assoc_index, set_index, 0, false, false, 0, nullptr, false);
            }
        }
    }
}

void Cache::debug_rword(Address source, void* destination, size_t size) const
{
    CacheLocation loc = compute_location(source);
    for (size_t assoc_index = 0; assoc_index < cache_config.associativity();
         assoc_index++) {
        if (dt[assoc_index][loc.set].valid
            && dt[assoc_index][loc.set].tag == loc.tag) {
            memory_copy(
                destination,
                (byte*)&dt[assoc_index][loc.set].data[loc.col] + loc.bytes,
                size);
            return;
        }
    }
    memory_set(destination, 0, size); // TODO is this correct
}

bool Cache::access(
    Address address,
    void* buffer,
    size_t size,
    AccessType access_type) const
{
    const CacheLocation loc = compute_location(address);
    size_t assoc_index = find_block_index(loc);

    // search failed - cache miss
    if (assoc_index >= cache_config.associativity()) {
        // if write through we do not need to allocate cache line does not
        // allocate
        if (access_type == WRITE
            && cache_config.write_policy() == CacheConfig::WP_THROUGH_NOALLOC) {
            miss_write++;
            emit miss_update(get_miss_count());
            update_all_statistics();
            return false;
        }

        assoc_index = replacement_policy->select_index_to_evict(loc.set);
        kick(assoc_index, loc.set);

        SANITY_ASSERT(
            assoc_index < cache_config.associativity(),
            "Probably unimplemented replacement policy");
    }

    struct CacheBlock& cd = dt[assoc_index][loc.set];

    // Update statistics and otherwise read from memory
    if (cd.valid) {
        if (access_type == WRITE) {
            hit_write++;
        } else {
            hit_read++;
        }
        emit hit_update(get_hit_count());
        update_all_statistics();
    } else {
        if (access_type == WRITE) {
            miss_write++;
        } else {
            miss_read++;
        }
        emit miss_update(get_miss_count());

        mem->read(
            calc_base_address(loc.tag, loc.set), cd.data.data(),
            cache_config.block_size() * BLOCK_ITEM_SIZE, { .debug = false });
        cd.valid = true;
        cd.dirty = false;
        cd.tag = loc.tag;

        change_counter += cache_config.block_size();
        mem_reads += cache_config.block_size();
        burst_reads += cache_config.block_size() - 1;
        emit memory_reads_update(mem_reads);
        update_all_statistics();
    }

    replacement_policy->update_stats(assoc_index, loc.set, cd.valid);

    const int64_t overlap = std::max(
        loc.col * BLOCK_ITEM_SIZE + size
            - cache_config.block_size() * BLOCK_ITEM_SIZE,
        { 0 });
    const size_t size1 = size - overlap;

    bool changed = false;

    if (access_type == READ) {
        memory_copy(buffer, &cd.data[loc.col] + loc.bytes, size1);
    } else if (access_type == WRITE) {
        cd.dirty = true;
        changed = !memory_compare(&cd.data[loc.col] + loc.bytes, buffer, size1);
        if (changed) {
            memory_copy(&cd.data[loc.col] + loc.bytes, buffer, size1);
            change_counter++;
        }
    }

    const auto last_affected_col
        = (loc.col * BLOCK_ITEM_SIZE + size1) / BLOCK_ITEM_SIZE;
    for (auto col = loc.col; col < last_affected_col; col++) {
        emit cache_update(
            assoc_index, loc.set, col, cd.valid, cd.dirty, cd.tag,
            cd.data.data(), access_type);
    }

    if (overlap > 0) {
        // If access overlaps single cache row, perform access to next row.
        changed |= access(
            address + (size1 * 8), (byte*)buffer + (size1 * 8), overlap,
            access_type);
    }

    return changed;
}

size_t Cache::find_block_index(const CacheLocation& loc) const
{
    uint32_t index = 0;
    while (index < cache_config.associativity()
           && (!dt[index][loc.set].valid || dt[index][loc.set].tag != loc.tag))
        index++;
    return index;
}

void Cache::kick(size_t assoc_index, size_t set_index) const
{
    struct CacheBlock& cd = dt[assoc_index][set_index];
    if (cd.dirty && cache_config.write_policy() == CacheConfig::WP_BACK) {
        mem->write(
            calc_base_address(cd.tag, set_index), cd.data.data(),
            cache_config.block_size() * BLOCK_ITEM_SIZE);
        mem_writes += cache_config.block_size();
        burst_writes += cache_config.block_size() - 1;
        emit memory_writes_update(mem_writes);
    }
    cd.valid = false;
    cd.dirty = false;

    change_counter++;

    replacement_policy->update_stats(assoc_index, set_index, false);
}

void Cache::update_all_statistics() const
{
    emit statistics_update(
        get_stall_count(), get_speed_improvement(), get_hit_rate());
}

Address Cache::calc_base_address(size_t tag, size_t set_index) const
{
    return Address(
        (tag * cache_config.set_count() + set_index) * cache_config.block_size()
        * BLOCK_ITEM_SIZE);
}

CacheLocation Cache::compute_location(Address address) const
{
    auto word_index = address.get_raw() / BLOCK_ITEM_SIZE;
    auto word_count = cache_config.block_size() * cache_config.set_count();
    auto index = word_index % word_count;
    return { .set = index / cache_config.block_size(),
             .col = index % cache_config.block_size(),
             .tag = word_index / word_count,
             .bytes = address.get_raw() % BLOCK_ITEM_SIZE };
}

enum LocationStatus Cache::location_status(Address address) const
{
    const CacheLocation loc = compute_location(address);

    if (cache_config.enabled()) {
        for (auto const& set : dt) {
            auto const& block = set[loc.set];

            if (block.valid && block.tag == loc.tag) {
                if (block.dirty
                    && cache_config.write_policy() == CacheConfig::WP_BACK) {
                    return (enum LocationStatus)(
                        LOCSTAT_CACHED | LOCSTAT_DIRTY);
                } else {
                    return LOCSTAT_CACHED;
                }
            }
        }
    }
    return mem->location_status(address);
}

const CacheConfig& Cache::get_config() const { return cache_config; }

uint32_t Cache::get_change_counter() const { return change_counter; }

uint32_t Cache::get_hit_count() const { return hit_read + hit_write; }

uint32_t Cache::get_miss_count() const { return miss_read + miss_write; }

uint32_t Cache::get_read_count() const { return mem_reads; }

uint32_t Cache::get_write_count() const { return mem_writes; }

uint32_t Cache::get_stall_count() const
{
    uint32_t st_cycles
        = mem_reads * (access_pen_r - 1) + mem_writes * (access_pen_w - 1);
    if (access_pen_b != 0) {
        st_cycles -= burst_reads * (access_pen_r - access_pen_b)
            + burst_writes * (access_pen_w - access_pen_b);
    }
    return st_cycles;
}

double Cache::get_speed_improvement() const
{
    uint32_t lookup_time;
    uint32_t mem_access_time;
    uint32_t comp = hit_read + hit_write + miss_read + miss_write;
    if (comp == 0) {
        return 100.0;
    }
    lookup_time = hit_read + miss_read;
    if (cache_config.write_policy() == CacheConfig::WP_BACK) {
        lookup_time += hit_write + miss_write;
    }
    mem_access_time = mem_reads * access_pen_r + mem_writes * access_pen_w;
    if (access_pen_b != 0) {
        mem_access_time -= burst_reads * (access_pen_r - access_pen_b)
            + burst_writes * (access_pen_w - access_pen_b);
    }
    return (
        (double)((miss_read + hit_read) * access_pen_r + (miss_write + hit_write) * access_pen_w)
        / (double)(lookup_time + mem_access_time) * 100);
}

double Cache::get_hit_rate() const
{
    uint32_t comp = hit_read + hit_write + miss_read + miss_write;
    if (comp == 0) {
        return 0.0;
    }
    return (double)(hit_read + hit_write) / (double)comp * 100.0;
}

}