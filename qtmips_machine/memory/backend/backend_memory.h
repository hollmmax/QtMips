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

#ifndef QTMIPS_MACHINE_BACKEND_MEMORY_H
#define QTMIPS_MACHINE_BACKEND_MEMORY_H

#include "../memory_utils.h"
#include <QObject>

namespace machine {

/**
 * Relative index within an instance of backend memory
 */
typedef uint64_t Offset;

/**
 * Interface for physical memory or periphery
 *
 * Properties:
 *  - Values returned have simulated endianness
 *  - Stored endianness is undefined
 */
class BackendMemory : public QObject {
    Q_OBJECT

public:
    /**
     * Write byte sequence to memory
     *
     * @param source        pointer to array of bytes to be copied
     * @param destination   relative index of destination to write to
     * @param size         number of bytes to be written
     * @return              true when memory before and after write differs
     */
    virtual WriteResult write(
        const void* source,
        Offset destination,
        size_t size,
        WriteOptions options)
        = 0;

    /**
     * Read sequence of bytes from memory
     *
     * @param source        relative index of data to be read
     * @param destination   pointer to destination buffer
     * @param size          number of bytes to be read
     * @param options       additional option like debug mode, see type
     * definition
     */
    virtual ReadResult read(
        Offset source,
        void* destination,
        size_t size,
        ReadOptions options) const = 0;

signals:
    /**
     * Notify upper layer about a change in managed physical memory of periphery
     *
     * @param mem_access    this
     * @param start_addr    affected area start
     * @param last_addr     affected area end
     * @param external      TODO
     */
    void external_backend_change_notify(
        const BackendMemory* mem_access,
        uint32_t start_addr,
        uint32_t last_addr,
        bool external) const;
};
}

#endif // QTMIPS_MACHINE_BACKEND_MEMORY_H
