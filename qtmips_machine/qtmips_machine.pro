QT -= gui

TARGET = qtmips_machine
CONFIG += c++14

TEMPLATE = lib
CONFIG += staticlib

LIBS += -lelf
QMAKE_CXXFLAGS += -std=c++14
QMAKE_CXXFLAGS_DEBUG += -ggdb
QMAKE_CXXFLAGS_DEBUG += -Wno-c99-designator

DEFINES += QTMIPS_MACHINE_LIBRARY
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
    qtmipsmachine.cpp \
    qtmipsexception.cpp \
    core.cpp \
    instruction.cpp \
    registers.cpp \
    programloader.cpp \
    alu.cpp \
    machineconfig.cpp \
    utils.cpp \
    symboltable.cpp \
    cop0state.cpp \
    memory/cache/cache.cpp \
    memory/cache/cache_policy.cpp \
    memory/mmu.cpp \
    memory/frontend_memory.cpp \
    memory/backend/memory.cpp \
    memory/backend/peripspiled.cpp \
    memory/backend/lcddisplay.cpp \
    memory/backend/peripheral.cpp \
    memory/backend/serialport.cpp

HEADERS += \
    qtmipsmachine.h \
    qtmipsexception.h \
    core.h \
    instruction.h \
    registers.h \
    programloader.h \
    alu.h \
    machineconfig.h \
    utils.h \
    machinedefs.h \
    symboltable.h \
    cop0state.h \
    memory/address.h \
    memory/cache/cache.h \
    memory/cache/cache_policy.h \
    memory/cache/cache_types.h \
    memory/memory_utils.h \
    memory/mmu.h \
    memory/frontend_memory.h \
    memory/backend/memory.h \
    memory/backend/peripspiled.h \
    memory/backend/lcddisplay.h \
    memory/backend/peripheral.h \
    memory/backend/serialport.h \
    memory/backend/backend_memory.h
