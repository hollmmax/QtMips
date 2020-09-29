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
#include "../memory_utils.h"
#include "../../qtmipsexception.h"
#include "../../utils.h"

namespace machine {


MemorySection::MemorySection(std::uint32_t length) {
    this->dt.resize(length, 0);
}

MemorySection::MemorySection(const MemorySection &other) {
    this->dt = other.dt;
}

bool MemorySection::write(
    void *source,
    Offset offset,
    size_t count
) {
    offset = offset >> 2U;

    if (offset >= this->len) {
        throw QTMIPS_EXCEPTION(
            OutOfMemoryAccess,
            "Trying to write outside of the memory section",
            QString("Accessing using offset: ") + QString::number(offset)
        );
    }

    bool changed = memory_compare(source, &this->dt[offset], count);
    if (changed) {
        memory_copy(&this->dt[offset], source, count);
    }
    return changed;
}

void MemorySection::read(
    Offset source,
    void *destination,
    size_t count,
    bool debug_read
) const {
    (void) debug_read;
    source = source >> 2U;

    if (source >= this->len) {
        throw QTMIPS_EXCEPTION(
            OutOfMemoryAccess,
            "Trying to read outside of the memory section",
            QString("Accessing using offset: ") + QString::number(source)
        );
    }

    memory_copy(destination, &this->dt[source], count);
}

size_t MemorySection::length() const {
    return this->dt->size();
}

const std::uint32_t *MemorySection::data() const {
    return this->dt->data();
}

bool MemorySection::operator==(const MemorySection &other) const {
    return this->dt == other.dt;
}

bool MemorySection::operator!=(const MemorySection &ms) const {
    return !this->operator==(ms);
}


//////////////////////////////////////////////////////////////////////////////
/// Some optimalization options
// How big memory sections will be in bits (2^6=64)
#define MEMORY_SECTION_BITS 6
// How big one row of lookup tree will be in bits (2^4=16)
#define MEMORY_TREE_BITS 4
//////////////////////////////////////////////////////////////////////////////
// Size of one section
#define MEMORY_SECTION_SIZE (1 << MEMORY_SECTION_BITS)
// Size of one memory row
#define MEMORY_TREE_ROW_SIZE (1 << MEMORY_TREE_BITS)
// Depth of tree
#define MEMORY_TREE_DEPTH ((30 - MEMORY_SECTION_BITS) / MEMORY_TREE_BITS)
// Just do some sanity checks
#if (MEMORY_SECTION_SIZE == 0)
    #error Nonzero memory section size is required
#endif
#if (MEMORY_TREE_ROW_SIZE == 0)
    #error Nonzero memory tree row size is required
#endif
#if (((30 - MEMORY_SECTION_BITS) % MEMORY_TREE_BITS) != 0)
    #error Number of bits in tree row has to be exact division of available number of bits
#endif

// Macro to generate mask of given size with given righ offset
#define GENMASK(SIZE, OFF) (((1 << (SIZE)) - 1) << (OFF))
// Get index in row for fiven offset and row number i
#define TREE_ROW_BIT_OFFSET(I) (30 - MEMORY_TREE_BITS - (I)*MEMORY_TREE_BITS)
#define TREE_ROW(OFFSET, I) (((OFFSET) & GENMASK(MEMORY_TREE_BITS, TREE_ROW_BIT_OFFSET(I))) >> TREE_ROW_BIT_OFFSET(I))

Memory::Memory() {
    this->mt_root = allocate_section_tree();
}

Memory::Memory(const Memory &m) {
    this->mt_root = copy_section_tree(m.get_memorytree_root(), 0);
    change_counter = 0;
    write_counter = 0;
}

Memory::~Memory() {
    free_section_tree(this->mt_root, 0);
    delete[] this->mt_root;
}

void Memory::reset() {
    free_section_tree(this->mt_root, 0);
    delete[] this->mt_root;
    this->mt_root = allocate_section_tree();
}

void Memory::reset(const Memory &m) {
    free_section_tree(this->mt_root, 0);
    this->mt_root = copy_section_tree(m.get_memorytree_root(), 0);
}

MemorySection *Memory::get_section(
    std::uint32_t address,
    bool create
) const {
    union MemoryTree *w = this->mt_root;
    size_t row_num;
    for (int i = 0; i < (MEMORY_TREE_DEPTH - 1); i++) {
        row_num = TREE_ROW(address, i);
        if (w[row_num].mt == nullptr) { // We don't have this tree so allocate it
            if (!create) { // If we shouldn't be creating it than just return null
                return nullptr;
            }
            w[row_num].mt = allocate_section_tree();
        }
        w = w[row_num].mt;
    }
    row_num = TREE_ROW(address, MEMORY_TREE_DEPTH - 1);
    if (w[row_num].sec == nullptr) {
        if (!create) {
            return nullptr;
        }
        w[row_num].sec = new MemorySection(MEMORY_SECTION_SIZE);
    }
    return w[row_num].sec;
}

#define SECTION_OFFSET_MASK(ADDR) (ADDR & GENMASK(MEMORY_SECTION_BITS, 2))

bool Memory::write(
    const void *source,
    Offset offset,
    size_t count
) {
    bool changed;
    MemorySection *section = this->get_section(offset, true);
    changed = section->write(source, SECTION_OFFSET_MASK(offset), count);
    write_counter++;
    if (changed) {
        change_counter++;
    }
    return changed;
}


void Memory::read(
    Offset source,
    void *destination,
    size_t count,
    bool debug_read
) const {
    MemorySection *section = this->get_section(source, false);
    if (section == nullptr) {
        memset(destination, 0, count);
    } else {
        return section->read(SECTION_OFFSET_MASK(source), destination, count, debug_read);
    }
}

bool Memory::operator==(const Memory &m) const {
    return compare_section_tree(this->mt_root, m.get_memorytree_root(), 0);
}

bool Memory::operator!=(const Memory &m) const {
    return !this->operator==(m);
}

const union machine::MemoryTree *Memory::get_memorytree_root() const {
    return this->mt_root;
}

union machine::MemoryTree *Memory::allocate_section_tree() {
    union MemoryTree *mt = new union MemoryTree[MEMORY_TREE_ROW_SIZE];
    memset(mt, 0, sizeof *mt * MEMORY_TREE_ROW_SIZE);
    return mt;
}

void Memory::free_section_tree(
    union machine::MemoryTree *mt,
    size_t depth
) {
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].mt != nullptr) {
                free_section_tree(mt[i].mt, depth + 1);
                delete[] mt[i].mt;
            }
        }
    } else { // Following level is memory section
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].sec != nullptr) {
                delete mt[i].sec;
            }
        }
    }
}

bool Memory::compare_section_tree(
    const union machine::MemoryTree *mt1,
    const union machine::MemoryTree *mt2,
    size_t depth
) {
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (((mt1[i].mt == nullptr || mt2[i].mt == nullptr) && mt1[i].mt != mt2[i].mt) || (
                mt1[i].mt != nullptr && mt2[i].mt != nullptr
                && !compare_section_tree(mt1[i].mt, mt2[i].mt, depth + 1))) {
                return false;
            }
        }
    } else { // Following level is memory section
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (((mt1[i].sec == nullptr || mt2[i].sec == nullptr) && mt1[i].sec != mt2[i].sec)
                || (mt1[i].sec != nullptr && mt2[i].sec != nullptr && *mt1[i].sec != *mt2[i].sec)) {
                return false;
            }
        }
    }
    return true;
}

union machine::MemoryTree *Memory::copy_section_tree(
    const union machine::MemoryTree *mt,
    size_t depth
) {
    union MemoryTree *nmt = allocate_section_tree();
    if (depth < (MEMORY_TREE_DEPTH - 1)) { // Following level is memory tree
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].mt != nullptr) {
                nmt[i].mt = copy_section_tree(mt[i].mt, depth + 1);
            }
        }
    } else { // Following level is memory section
        for (int i = 0; i < MEMORY_TREE_ROW_SIZE; i++) {
            if (mt[i].sec != nullptr) {
                nmt[i].sec = new MemorySection(*mt[i].sec);
            }
        }
    }
    return nmt;
}


}
