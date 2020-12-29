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

#include "../machinedefs.h"
#include "../memory/backend/memory.h"
#include "../memory/memory_utils.h"
#include "tst_machine.h"

using namespace machine;

void MachineTests::memory_data() {
    QTest::addColumn<uint32_t>("address");

    QTest::newRow("memory begin") << (uint32_t)0x00;
    QTest::newRow("memory end") << (uint32_t)0xFFFFFFFC;
    QTest::newRow("memory midle start") << (uint32_t)0xFFFF00;
    QTest::newRow("memory midle end") << (uint32_t)0xFFFFFF;
}

void MachineTests::memory() {
    Memory m;

    QFETCH(uint32_t, address);

    // Uninitialize memory should read as zero
    QCOMPARE(memory_read_u8(&m, address), (uint8_t)0);
    QCOMPARE(memory_read_u16(&m, address), (uint16_t)0);
    QCOMPARE(memory_read_u32(&m, address), (uint32_t)0);
    // Just a byte
    memory_write_u8(&m, address, 0x42);
    QCOMPARE(memory_read_u8(&m, address), (uint8_t)0x42);
    // Half word
    memory_write_u16(&m, address, 0x4243);
    QCOMPARE(memory_read_u16(&m, address), (uint16_t)0x4243);
    // Word
    memory_write_u32(&m, address, 0x42434445);
    QCOMPARE(memory_read_u32(&m, address), (uint32_t)0x42434445);
}

void MachineTests::memory_section_data() {
    QTest::addColumn<uint32_t>("address");

    QTest::newRow("memory begin") << (uint32_t)0x00;
    QTest::newRow("memory end") << (uint32_t)0xFFFFFFFF;
    QTest::newRow("memory midle start") << (uint32_t)0xFFFF00;
    QTest::newRow("memory midle end") << (uint32_t)0xFFFFFF;
}

void MachineTests::memory_section() {
    Memory m;

    QFETCH(uint32_t, address);

    // First section shouldn't exists
    QCOMPARE(m.get_section(address, false), (MemorySection*)nullptr);

    // Create section
    MemorySection* s = m.get_section(address, true);
    QVERIFY(s != nullptr);

    // Write some data to memory
    memory_write_u8(&m, address, 0x42);
    // Read it trough section (mask bits outside of the memory section)
    QCOMPARE(memory_read_u8(s, address & 0xFF), (uint8_t)0x42);
    // Write some other data trough section
    memory_write_u8(s, address & 0xFF, 0x66);
    // Read trough memory
    QCOMPARE(memory_read_u8(&m, address), (uint8_t)0x66);
}

void MachineTests::memory_endian() {
    Memory m;

    // Memory should be big endian so write bytes from most significant byte
    memory_write_u8(&m, 0x00, 0x12);
    memory_write_u8(&m, 0x01, 0x34);
    memory_write_u8(&m, 0x02, 0x56);
    memory_write_u8(&m, 0x03, 0x78);
    QCOMPARE(memory_read_u16(&m, 0x00), (uint16_t)0x1234);
    QCOMPARE(memory_read_u32(&m, 0x00), (uint32_t)0x12345678);

    memory_write_u16(&m, 0x80, 0x1234);
    QCOMPARE(memory_read_u8(&m, 0x80), (uint8_t)0x12);
    QCOMPARE(memory_read_u8(&m, 0x81), (uint8_t)0x34);

    memory_write_u32(&m, 0xF0, 0x12345678);
    QCOMPARE(memory_read_u8(&m, 0xF0), (uint8_t)0x12);
    QCOMPARE(memory_read_u8(&m, 0xF1), (uint8_t)0x34);
    QCOMPARE(memory_read_u8(&m, 0xF2), (uint8_t)0x56);
    QCOMPARE(memory_read_u8(&m, 0xF3), (uint8_t)0x78);
}

void MachineTests::memory_compare() {
    Memory m1, m2;
    QCOMPARE(m1, m2);
    memory_write_u8(&m1, 0x20, 0x0);
    QVERIFY(m1 != m2); // This should not be equal as this identifies also
                       // memory write (difference between no write and zero
                       // write)
    memory_write_u8(&m1, 0x20, 0x24);
    QVERIFY(m1 != m2);
    memory_write_u8(&m2, 0x20, 0x23);
    QVERIFY(m1 != m2);
    memory_write_u8(&m2, 0x20, 0x24);
    QCOMPARE(m1, m2);
    // Do the same with some other section
    memory_write_u8(&m1, 0xFFFF20, 0x24);
    QVERIFY(m1 != m2);
    memory_write_u8(&m2, 0xFFFF20, 0x24);
    QCOMPARE(m1, m2);
    // And also check memory copy
    Memory m3(m1);
    QCOMPARE(m1, m3);
    memory_write_u8(&m3, 0x18, 0x22);
    QVERIFY(m1 != m3);
}
