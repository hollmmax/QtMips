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

#ifndef LCDDISPLAY_H
#define LCDDISPLAY_H

#include "../../machinedefs.h"
#include "../../qtmipsexception.h"
#include "backend_memory.h"
#include <QMap>
#include <QObject>
#include <cstdint>

namespace machine {

class LcdDisplay final : public BackendMemory {
    Q_OBJECT
public:
    LcdDisplay();
    ~LcdDisplay() override;

signals:
    void write_notification(Offset offset, uint32_t value) const;
    void read_notification(Offset offset, uint32_t value) const;
    void pixel_update(size_t x, size_t y, uint r, uint g, uint b);

public:
    WriteResult write(
        const void* source,
        Offset destination,
        size_t size,
        WriteOptions options) override;

    ReadResult read(
        Offset source,
        void* destination,
        size_t size,
        ReadOptions options) const override;

    /**
     * @return  framebuffer width in pixels
     */
    inline constexpr size_t get_width() const { return fb_width; }

    /**
     * @return  framebuffer height in pixels
     */
    inline constexpr size_t get_height() const { return fb_height; }

private:
    /**
     * Read HW register - allows only 32bit alligned access.
     */
    uint32_t read_reg(Offset source) const;

    /**
     * Write HW register - allows only 32bit alligned access.
     */
    bool write_reg(Offset destination, uint32_t value);

    size_t get_fb_line_size() const;
    size_t get_fb_size() const;
    size_t get_address_from_pixel(size_t x, size_t y) const;
    std::tuple<size_t, size_t> get_pixel_from_address(size_t address) const;

    const size_t fb_width;  //> Width in pixels
    const size_t fb_height; //> Height in pixels
    const size_t fb_bits_per_pixel;
    std::vector<byte> fb_data;
};

}

#endif // LCDDISPLAY_H
