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

#include "memory.h"
#include "../../qtmipsexception.h"
#include <memory>

namespace machine {

MemorySection::MemorySection(size_t length_bytes)
    : dt(length_bytes, 0)
{
}

MemorySection::MemorySection(const MemorySection& other)
    : dt(other.dt)
{
}

WriteResult MemorySection::write(
    const void* source,
    Offset offset,
    size_t size,
    WriteOptions options)
{
    UNUSED(options)

    if (offset >= this->length()) {
        throw QTMIPS_EXCEPTION(
            OutOfMemoryAccess, "Trying to write outside of the memory section",
            QString("Accessing using offset: ") + QString::number(offset));
    }

    // Size the can be read from this section
    const size_t available_size
        = std::min(destination + size, this->length()) - destination;

    // TODO, make swap conditional for big endian machies
    bool changed
        = !memory_compare(source, &this->dt[destination], available_size);
    if (changed) {
        memory_copy(&this->dt[destination], source, available_size);
    }

    return { .n_bytes = available_size, .changed = changed };
}

ReadResult MemorySection::read(
    Offset source,
    void* destination,
    size_t size,
    ReadOptions options) const
{
    UNUSED(options)

    size = std::min(source + size, this->length()) - source;

    if (source >= this->length()) {
        throw QTMIPS_EXCEPTION(
            OutOfMemoryAccess, "Trying to read outside of the memory section",
            QString("Accessing using offset: ") + QString::number(source));
    }

    memory_copy(destination, &this->dt[source], size);

    return { .n_bytes = size };
}

size_t MemorySection::length() const { return this->dt.size(); }

const byte* MemorySection::data() const { return this->dt.data(); }

bool MemorySection::operator==(const MemorySection& other) const
{
    return this->dt == other.dt;
}

bool MemorySection::operator!=(const MemorySection& ms) const
{
    return !this->operator==(ms);
}

// Settings sanity checks
static_assert(
    MEMORY_SECTION_SIZE != 0,
    "Nonzero memory section size is required.");
static_assert(
    MEMORY_TREE_ROW_SIZE != 0,
    "Nonzero memory tree row size is required.");
static_assert(
    ((32 - MEMORY_SECTION_BITS) % MEMORY_TREE_BITS) == 0,
    "Number of bits in tree row has to be exact division of available number "
    "of bits.");

/**
 * Generate mask of given size with given righ offset
 */
constexpr uint64_t genmask(size_t size, size_t offset)
{
    return ((1u << size) - 1) << offset;
}

/**
 * Get index in row for fiven offset and row number i
 */
constexpr size_t tree_row_bit_offset(size_t i)
{
    return 32 - MEMORY_TREE_BITS - i * MEMORY_TREE_BITS;
}

constexpr size_t get_tree_row(size_t offset, size_t i)
{
    return (offset & genmask(MEMORY_TREE_BITS, tree_row_bit_offset(i)))
        >> tree_row_bit_offset(i);
}

Memory::Memory() { this->mt_root = allocate_section_tree(); }

Memory::Memory(const Memory& m)
{
    this->mt_root = copy_section_tree(m.get_memorytree_root(), 0);
    change_counter = 0;
    write_counter = 0;
}

Memory::~Memory()
{
    free_section_tree(this->mt_root, 0);
    delete[] this->mt_root;
}

void Memory::reset()
{
    free_section_tree(this->mt_root, 0);
    delete[] this->mt_root;
    this->mt_root = allocate_section_tree();
}

void Memory::reset(const Memory& m)
{
    free_section_tree(this->mt_root, 0);
    this->mt_root = copy_section_tree(m.get_memorytree_root(), 0);
}

MemorySection* Memory::get_section(std::uint32_t address, bool create) const
{
    union MemoryTree* w = this->mt_root;
    size_t row_num;
    for (size_t i = 0; i < (MEMORY_TREE_DEPTH - 1); i++) {
        row_num = get_tree_row(address, i);
        if (w[row_num].subtree == nullptr) { // We don't have this tree so
                                             // allocate
                                             // it
            if (!create) { // If we shouldn't be creating it than just return
                // null
                return nullptr;
            }
            w[row_num].subtree = allocate_section_tree();
        }
        w = w[row_num].subtree;
    }
    row_num = get_tree_row(address, MEMORY_TREE_DEPTH - 1);
    if (w[row_num].sec == nullptr) {
        if (!create) {
            return nullptr;
        }
        w[row_num].sec = new MemorySection(MEMORY_SECTION_SIZE);
    }
    return w[row_num].sec;
}

size_t get_section_offset_mask(size_t addr)
{
    return addr & genmask(MEMORY_SECTION_BITS, 0);
}

WriteResult Memory::write(
    const void* source,
    Offset offset,
    size_t size,
    WriteOptions options)
{
    UNUSED(options)

    WriteResult result {};
    MemorySection* section = this->get_section(offset, true);
    result = section->write(
        source, get_section_offset_mask(offset), size, WriteOptions());
    write_counter++;
    if (result.changed) {
        change_counter++;
    }
    return result;
}

ReadResult Memory::read(
    Offset source,
    void* destination,
    size_t size,
    ReadOptions options) const
{
    MemorySection* section = this->get_section(source, false);
    if (section == nullptr) {
        memset(destination, 0, size);
    } else {
        return section->read(
            get_section_offset_mask(source), destination, size, options);
    }
    return { .n_bytes = size };
}

bool Memory::operator==(const Memory& m) const
{
    return compare_section_tree(this->mt_root, m.get_memorytree_root(), 0);
}

bool Memory::operator!=(const Memory& m) const { return !this->operator==(m); }

const union machine::MemoryTree* Memory::get_memorytree_root() const
{
    return this->mt_root;
}

union machine::MemoryTree* Memory::allocate_section_tree()
{
    auto* mt = new union MemoryTree[MEMORY_TREE_ROW_SIZE];
    memset(mt, 0, sizeof *mt * MEMORY_TREE_ROW_SIZE);
    return mt;
}

void Memory::free_section_tree(union MemoryTree* mt, size_t depth)
{
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].subtree != nullptr) {
                free_section_tree(mt[i].subtree, depth + 1);
                delete[] mt[i].subtree;
            }
        }
    } else { // Following level is memory section
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            delete mt[i].sec;
        }
    }
    delete mt;
}

bool Memory::compare_section_tree(
    const union MemoryTree* mt1,
    const union MemoryTree* mt2,
    size_t depth)
{
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (((mt1[i].subtree == nullptr || mt2[i].subtree == nullptr)
                 && mt1[i].subtree != mt2[i].subtree)
                || (mt1[i].subtree != nullptr && mt2[i].subtree != nullptr
                    && !compare_section_tree(
                        mt1[i].subtree, mt2[i].subtree, depth + 1))) {
                return false;
            }
        }
    } else { // Following level is memory section
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (((mt1[i].sec == nullptr || mt2[i].sec == nullptr)
                 && mt1[i].sec != mt2[i].sec)
                || (mt1[i].sec != nullptr && mt2[i].sec != nullptr
                    && *mt1[i].sec != *mt2[i].sec)) {
                return false;
            }
        }
    }
    return true;
}

union machine::MemoryTree*
Memory::copy_section_tree(const union MemoryTree* mt, size_t depth)
{
    union MemoryTree* nmt = allocate_section_tree();
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].subtree != nullptr) {
                nmt[i].subtree = copy_section_tree(mt[i].subtree, depth + 1);
            }
        }
    } else { // Following level is memory section
        for (size_t i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].sec != nullptr) {
                nmt[i].sec = new MemorySection(*mt[i].sec);
            }
        }
    }
    return nmt;
}

}
