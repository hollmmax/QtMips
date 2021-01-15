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

#ifndef QTMIPS_MAPPED_MEMORY_H
#define QTMIPS_MAPPED_MEMORY_H

#include "../address.h"
#include "../memory_utils.h"
#include "./backend_memory.h"
#include <QFileDevice>

namespace machine {

class MappedMemory final : public BackendMemory {
public:
    explicit MappedMemory(byte* data, size_t length_bytes, QFileDevice* parent);
    ~MappedMemory() override;

    WriteResult write(
        Offset destination,
        const void* source,
        size_t total_size,
        WriteOptions options) override;

    ReadResult read(
        void* destination,
        Offset source,
        size_t size,
        ReadOptions options) const override;

    size_t length() const;
    const byte* data() const;

private:
    byte* dt;
    size_t length_bytes;
    QFileDevice* parent;
};

} // namespace machine

#endif // QTMIPS_MAPPED_MEMORY_H
