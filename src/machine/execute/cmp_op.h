#ifndef CMP_OP_H
#define CMP_OP_H

#include <QMetaType>

namespace machine {

enum class CmpOp : uint8_t {
    BEQ = 0b000,
    BNE = 0b001,
    BLT = 0b100,
    BGE = 0b101,
    BLTU = 0b110,
    BGEU = 0b111,
};

}

Q_DECLARE_METATYPE(machine::CmpOp)

#endif // CMP_OP_H
