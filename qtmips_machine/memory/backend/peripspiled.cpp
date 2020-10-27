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

#include "peripspiled.h"

using namespace machine;

constexpr size_t SPILED_REG_LED_LINE_o = 0x004;
constexpr size_t SPILED_REG_LED_RGB1_o = 0x010;
constexpr size_t SPILED_REG_LED_RGB2_o = 0x014;
constexpr size_t SPILED_REG_LED_KBDWR_DIRECT_o = 0x018;

constexpr size_t SPILED_REG_KBDRD_KNOBS_DIRECT_o = 0x020;
constexpr size_t SPILED_REG_KNOBS_8BIT_o = 0x024;

PeripSpiLed::PeripSpiLed() = default;

PeripSpiLed::~PeripSpiLed() = default;

WriteResult PeripSpiLed::write(const void* source, Offset destination, size_t size)
{
    size_t remaining_size = size;
    Offset current_dest = destination;
    const byte* current_source = static_cast<const byte*>(source);

    WriteResult result {};

    do {
        const size_t inword_offset = current_dest & 3u;
        const size_t parial_size = std::min(remaining_size, sizeof(uint32_t) - inword_offset);
        WriteResult partial_result { .n_bytes = parial_size, .changed = true };

        switch (destination & ~3u) {
        case SPILED_REG_LED_LINE_o: {
            if (!memory_compare_and_copy(&spiled_reg_led_line, current_source, parial_size, inword_offset)) {
                emit led_line_changed(spiled_reg_led_line);
            }
            break;
        }
        case SPILED_REG_LED_RGB1_o:
            if (!memory_compare_and_copy(&spiled_reg_led_rgb1, current_source, parial_size, inword_offset)) {
                emit led_rgb1_changed(spiled_reg_led_rgb1);
            }
            break;
        case SPILED_REG_LED_RGB2_o:
            if (!memory_compare_and_copy(&spiled_reg_led_rgb2, current_source, parial_size, inword_offset)) {
                emit led_rgb2_changed(spiled_reg_led_rgb2);
            }
            break;
        default:
            // Todo show this to user as this is failure of supplied program
            printf("[WARNING] PeripSpiLed: write to non-writable location.\n");
            partial_result.n_bytes = 0;
            partial_result.changed = false;
            break;
        }

        result += partial_result;
        destination += current_dest;
        current_source += parial_size;
        remaining_size -= parial_size;
    } while (remaining_size > 0);

    if (result.changed) {
        emit write_notification(destination, size);
    }

    return result;
}

ReadResult PeripSpiLed::read(Offset source, void* destination, size_t size, ReadOptions options) const
{
    UNUSED(options)

    size_t remaining_size = size;
    Offset current_source = source;
    byte* current_dest = static_cast<byte*>(destination);

    ReadResult result {};

    do {
        const size_t inword_offset = source & 3u;
        const size_t partial_size = std::min(remaining_size, sizeof(uint32_t) - inword_offset);
        ReadResult partial_result {};

        uint32_t const* partial_source = [&]() {
            switch (current_source & ~3u) {
            case SPILED_REG_LED_LINE_o:
                return &spiled_reg_led_line;
            case SPILED_REG_LED_RGB1_o:
                return &spiled_reg_led_rgb1;
            case SPILED_REG_LED_RGB2_o:
                return &spiled_reg_led_rgb2;
            case SPILED_REG_LED_KBDWR_DIRECT_o:
                return &spiled_reg_led_kbdwr_direct;
            case SPILED_REG_KBDRD_KNOBS_DIRECT_o:
                return &spiled_reg_kbdrd_knobs_direct;
            case SPILED_REG_KNOBS_8BIT_o:
                return &spiled_reg_knobs_8bit;
            default:
                return (const uint32_t*)nullptr;
            }
        }();

        if (partial_source != nullptr) {
            memory_copy(current_dest, (byte*)partial_source + inword_offset, partial_size);
        } else {
            // Todo show this to user as this is failure of supplied program
            printf("[WARNING] PeripSpiLed: read to non-readable location.\n");
        }

        result += partial_result;
        current_source += partial_size;
        source += partial_size;
        remaining_size -= partial_size;

    } while (remaining_size > 0);

    emit read_notification(source, size);

    return result;
}

void PeripSpiLed::knob_update_notify(uint32_t val, uint32_t mask, size_t shift)
{
    mask <<= shift;
    val <<= shift;

    if (!((spiled_reg_knobs_8bit ^ val) & mask)) {
        return;
    }

    spiled_reg_knobs_8bit &= ~mask;
    spiled_reg_knobs_8bit |= val;

    emit external_backend_change_notify(this, SPILED_REG_KNOBS_8BIT_o,
        SPILED_REG_KNOBS_8BIT_o + 3, true);
}

void PeripSpiLed::red_knob_update(int val)
{
    knob_update_notify(val, 0xff, 16);
}

void PeripSpiLed::green_knob_update(int val)
{
    knob_update_notify(val, 0xff, 8);
}

void PeripSpiLed::blue_knob_update(int val)
{
    knob_update_notify(val, 0xff, 0);
}

void PeripSpiLed::red_knob_push(bool state)
{
    knob_update_notify(state ? 1 : 0, 1, 26);
}

void PeripSpiLed::green_knob_push(bool state)
{
    knob_update_notify(state ? 1 : 0, 1, 25);
}

void PeripSpiLed::blue_knob_push(bool state)
{
    knob_update_notify(state ? 1 : 0, 1, 24);
}
