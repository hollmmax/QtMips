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

// TODO this file was created by copying memory.h, and wasn't fully cleaned afterwards

#ifndef QTMIPS_MAPPED_MEMORY_H
#define QTMIPS_MAPPED_MEMORY_H

#include "backend_memory.h"
#include "../address.h"
#include <QFileDevice>

namespace machine {

class MappedMemory : public BackendMemory {
public:
    explicit MappedMemory(QFileDevice* file, size_t length, size_t offset);
    ~MappedMemory() override;

    /**
     * Write byte sequence to memory
     *
     * @param source        pointer to array of bytes to be copied
     * @param destination   relative index of destination to write to
     * @param size         number of bytes to be written
     * @return              true when memory before and after write differs
     */
    bool write(
        const void *source,
        Offset destination,
        size_t size
    ) override;

    /**
     * Read sequence of bytes from memory
     *
     * @param source        relative index of data to be read
     * @param destination   pointer to destination buffer
     * @param size         number of bytes to be read
     * @param debug_read    TODO
     */
    void read(
        Offset source,
        void *destination,
        size_t size,
        bool debug_read = false
    ) const override;

    size_t length() const;
    const uchar* data() const;

    // TODO maybe impl Eq?

private:
    QFileDevice* file;
    uchar* dt;
    size_t len;

};
};

#endif //QTMIPS_MAPPED_MEMORY_H
