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

#include "mmu.h"
#include "memory_utils.h"

using namespace machine;

MMU::MMU()
    : change_counter(0)
{
}

MMU::~MMU()
{
    while (!ranges_by_access.isEmpty()) {
        RangeDesc* p_range = ranges_by_addr.first();
        ranges_by_addr.remove(p_range->last_addr);
        ranges_by_access.remove(p_range->backend_memory);
        if (p_range->owned) {
            delete p_range->backend_memory;
        }
        delete p_range;
    }
}

WriteResult MMU::write(
    Address destination,
    const void* source,
    size_t size)
{
    RangeDesc* p_range = find_range(destination);
    if (p_range == nullptr) {
        return { .n_bytes = 0, .changed = false };
    }
    int64_t overlap = p_range->last_addr - destination + size * 8;
    Offset dest_offset = destination - p_range->start_addr;

    WriteResult result {};
    if (overlap <= 0) {
        result = p_range->backend_memory->write(
            source, dest_offset, size, WriteOptions());
        if (result.changed) {
            change_counter++;
        }
    } else {
        size_t size1 = size - overlap; // Size that fits into this range.
        result = p_range->backend_memory->write(
            source, dest_offset, size1, WriteOptions());
        if (result.changed) {
            change_counter++;
        }
        result += this->write(
            destination + size1, (byte*)source + size1, overlap);
    }

    return result;
}

ReadResult MMU::read(
    Address source,
    void* destination,
    size_t size,
    ReadOptions options) const
{
    const RangeDesc* p_range = find_range(source);
    if (p_range == nullptr) {
        memory_set(destination, 0, size);
        return { size };
    }

    int64_t overlap = p_range->last_addr - source + size * 8;

    if (overlap <= 0) {
        /* Read fits into single range */
        p_range->backend_memory->read(
            source - p_range->start_addr,
            destination, size, options);
    } else {
        /* Read needs to be split */
        p_range->backend_memory->read(
            source - p_range->start_addr,
            destination, size - overlap, options);

        this->read(
            source + size - overlap,
            (byte*)destination + size - overlap,
            overlap, options);
    }
    return {};
}

std::uint32_t MMU::get_change_counter() const
{
    return change_counter;
}

enum LocationStatus MMU::location_status(Address address) const
{
    const RangeDesc* p_range = find_range(address);
    if (p_range == nullptr) {
        return LOCSTAT_ILLEGAL;
    }
    return LOCSTAT_NONE;
    //    return p_range->backend_memory->location_status(Address(address - p_range->start_addr)); // TODO: Where to store?
    // spaces
}

MMU::RangeDesc* MMU::find_range(Address address) const
{
    MMU::RangeDesc* p_range;
    auto i = ranges_by_addr.lowerBound(address);
    if (i == ranges_by_addr.end()) {
        return nullptr;
    }
    p_range = i.value();
    if (address >= p_range->start_addr && address <= p_range->last_addr) {
        return p_range;
    }
    return nullptr;
}

bool MMU::insert_range(
    BackendMemory* mem_access,
    Address start_addr,
    Address last_addr,
    bool move_ownership)
{
    auto* p_range = new RangeDesc(mem_access, start_addr, last_addr, move_ownership);
    auto i = ranges_by_addr.lowerBound(start_addr);
    if (i != ranges_by_addr.end()) {
        if (i.value()->start_addr <= last_addr && i.value()->last_addr >= start_addr) {
            return false;
        }
    }
    ranges_by_addr.insert(last_addr, p_range);
    ranges_by_access.insert(mem_access, p_range);
    //    connect(
    //        mem_access, &BackendMemory::external_change_notify,this, SLOT(
    //        range_external_change(const FrontendMemory*, std::uint32_t, std::uint32_t, bool)));
    return true;
}

bool MMU::remove_range(BackendMemory* mem_acces)
{
    RangeDesc* p_range = ranges_by_access.take(mem_acces);
    if (p_range == nullptr) {
        return false;
    }
    ranges_by_addr.remove(p_range->last_addr);
    if (p_range->owned) {
        delete p_range->backend_memory;
    }
    delete p_range;
    return true;
}

void MMU::clean_range(
    Address start_addr,
    Address last_addr)
{
    auto i = ranges_by_addr.lowerBound(start_addr);
    while (i != ranges_by_addr.end()) {
        RangeDesc* p_range = i.value();
        i++;
        if (p_range->start_addr <= last_addr) {
            remove_range(p_range->backend_memory);
        } else {
            break;
        }
    }
}

void MMU::range_backend_external_change(
    const BackendMemory* mem_access,
    Offset start_offset,
    Offset last_offset,
    bool external)
{
    if (external) {
        change_counter++;
    }
    auto i = ranges_by_access.find(const_cast<BackendMemory*>(mem_access));
    while (i != ranges_by_access.end() && i.key() == mem_access) {
        RangeDesc* p_range = i.value();
        ++i;
        emit external_change_notify(
            this, p_range->start_addr + start_offset, p_range->start_addr + last_offset, external);
    }
}

MMU::RangeDesc::RangeDesc(
    BackendMemory* mem_access,
    Address start_addr,
    Address last_addr,
    bool owned)
    : backend_memory(mem_access)
    , start_addr(start_addr)
    , last_addr(last_addr)
    , owned(owned)
{
}
