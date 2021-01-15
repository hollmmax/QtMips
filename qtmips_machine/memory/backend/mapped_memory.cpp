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

#include "mapped_memory.h"
#include "../../qtmipsexception.h"
#include <memory>

namespace machine {

MappedMemory::MappedMemory(byte* data, size_t length_bytes, QFileDevice* parent)
    : dt(data), length_bytes(length_bytes), parent(parent)
{
}

MappedMemory::~MappedMemory() {
    this->parent->unmap(this->dt);
}

WriteResult MappedMemory::write(
    Offset destination,
    const void* source,
    size_t size,
    WriteOptions options) {
    UNUSED(options)

    if (destination >= this->length()) {
        throw QTMIPS_EXCEPTION(
            OutOfMemoryAccess, "Trying to write outside of the memory section",
            QString("Accessing using offset: ") + QString::number(destination));
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

ReadResult MappedMemory::read(
    void* destination,
    Offset source,
    size_t size,
    ReadOptions options) const {
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

size_t MappedMemory::length() const { return this->length_bytes; }

const byte* MappedMemory::data() const { return this->dt; }

}
