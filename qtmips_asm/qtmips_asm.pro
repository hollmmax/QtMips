QT -= gui

TARGET = qtmips_asm
CONFIG += c++14

TEMPLATE = lib
CONFIG += staticlib

INCLUDEPATH += $$PWD/../qtmips_machine $$PWD/../qtmips_osemu
DEPENDPATH += $$PWD/../qtmips_machine

LIBS += -lelf
QMAKE_CXXFLAGS += -std=c++14
QMAKE_CXXFLAGS_DEBUG += -ggdb
QMAKE_CXXFLAGS_DEBUG += -Wno-c99-designator

DEFINES += QTMIPS_MACHINE_LIBRARY
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
    fixmatheval.cpp \
    simpleasm.cpp

HEADERS += \
    fixmatheval.h \
    messagetype.h \
    simpleasm.h
