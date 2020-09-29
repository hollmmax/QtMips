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

#include "frontend_memory.h"
#include "../register_value.h"


using namespace machine;

bool FrontendMemory::write_byte(
    Address address,
    uint8_t value
) {
    return write_generic<typeof(value)>(address, value);
}

bool FrontendMemory::write_hword(
    Address address,
    uint16_t value
) {
    return write_generic<typeof(value)>(address, value);
}

bool FrontendMemory::write_word(
    Address address,
    std::uint32_t value
) {
    return write_generic<typeof(value)>(address, value);
}

bool FrontendMemory::write_dword(
    Address address,
    uint64_t value
) {
    return write_generic<typeof(value)>(address, value);
}

uint8_t FrontendMemory::read_byte(
    Address address,
    bool debug_access
) const {
    return read_generic<uint8_t>(address, debug_access);
}

uint16_t FrontendMemory::read_hword(
    Address address,
    bool debug_access
) const {
    return read_generic<uint16_t>(address, debug_access);
}

uint32_t FrontendMemory::read_word(
    Address address,
    bool debug_access
) const {
    return read_generic<uint32_t>(address, debug_access);
}

uint64_t FrontendMemory::read_dword(
    Address address,
    bool debug_access
) const {
    return read_generic<uint64_t>(address, debug_access);
}

void FrontendMemory::write_ctl(
    enum AccessControl ctl,
    Address offset,
    RegisterValue value
) {
    switch (ctl) {
        case AC_NONE: {
            break;
        }
        case AC_BYTE:
        case AC_BYTE_UNSIGNED: {
            write_generic<uint8_t>(offset, (uint8_t) value.as_u32());
            break;
        }
        case AC_HALFWORD:
        case AC_HALFWORD_UNSIGNED: {
            write_generic<uint16_t>(offset, (uint16_t) value.as_u32());
            break;
        }
        case AC_WORD: {
            write_generic<uint32_t>(offset, (uint32_t) value.as_u32());
            break;
        }
        default: {

            throw QTMIPS_EXCEPTION(
                UnknownMemoryControl,
                "Trying to write to memory with unknown ctl",
                QString::number(ctl)
            );
        }
    }
}

RegisterValue FrontendMemory::read_ctl(
    enum AccessControl ctl,
    Address address
) const {
    switch (ctl) {
        case AC_NONE:
            return 0;
        case AC_BYTE:
            return read_generic<int8_t>(address, false);
        case AC_HALFWORD:
            return read_generic<int16_t>(address, false);
        case AC_WORD:
            return read_generic<int32_t>(address, false);
        case AC_BYTE_UNSIGNED:
            return read_generic<uint8_t>(address, false);
        case AC_HALFWORD_UNSIGNED:
            return read_generic<uint16_t>(address, false);
        default: {
            throw QTMIPS_EXCEPTION(
                UnknownMemoryControl,
                "Trying to read from memory with unknown ctl",
                QString::number(ctl)
            );
        }
    }
}

void FrontendMemory::sync() {}

enum LocationStatus FrontendMemory::location_status(Address address) const {
    (void) address;
    return LOCSTAT_NONE;
}

template<typename T>
T FrontendMemory::read_generic(
    Address address,
    bool debug_read
) const {
    if (!address.is_aligned<T>()) {
        // TODO: should we check alignment
    }

    T value;
    read(address, &value, sizeof(T));
    return value;
}

template<typename T>
bool FrontendMemory::write_generic(
    Address address,
    const T value
) {
    if (!address.is_aligned<T>()) {
        // TODO: should we check alignment
    }
    return write(address, &value, sizeof(T));
}