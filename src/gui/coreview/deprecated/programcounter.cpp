#include "programcounter.h"

#include "coreview_colors.h"
#include "fontsize.h"

#include <cmath>

using namespace coreview;

//////////////////////
#define WIDTH 72
#define HEIGHT 25
#define PENW 1
//////////////////////

ProgramCounter::ProgramCounter(machine::Machine *machine)
    : QGraphicsObject(nullptr)
    , name("", this)
    , value(this) {
    registers = machine->registers();

    QFont font;

    font.setPixelSize(FontSize::SIZE7);
    name.setPos(WIDTH / 2 - name.boundingRect().width() / 2, 0);
    name.setFont(font);
    font.setPointSize(FontSize::SIZE8);
    value.setText(
        QString(" 0x")
        + QString::number(machine->registers()->read_pc().get_raw(), 16));
    value.setPos(1, HEIGHT - value.boundingRect().height());
    value.setFont(font);

    connect(
        machine->registers(), &machine::Registers::pc_update, this,
        &ProgramCounter::pc_update);

    con_in = new Connector(Connector::AX_Y);
    con_out = new Connector(Connector::AX_Y);
    setPos(x(), y()); // To set initial connectors positions
}

ProgramCounter::~ProgramCounter() {
    delete con_in;
    delete con_out;
}

QRectF ProgramCounter::boundingRect() const {
    return { -PENW / 2, -PENW / 2, WIDTH + PENW, HEIGHT + PENW };
}

void ProgramCounter::paint(
    QPainter *painter,
    const QStyleOptionGraphicsItem *option __attribute__((unused)),
    QWidget *widget __attribute__((unused))) {
    QPen pen = painter->pen();
    pen.setColor(BLOCK_OUTLINE_COLOR);
    painter->setPen(pen);

    painter->drawRect(0, 0, WIDTH, HEIGHT);
}

void ProgramCounter::setPos(qreal x, qreal y) {
    QGraphicsObject::setPos(x, y);
    con_in->setPos(x + WIDTH / 2, y + HEIGHT);
    con_out->setPos(x + WIDTH / 2, y);
}

const Connector *ProgramCounter::connector_in() const {
    return con_in;
}

const Connector *ProgramCounter::connector_out() const {
    return con_out;
}

void ProgramCounter::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event
                                           __attribute__((unused))) {
    emit open_program();
    emit jump_to_pc(registers->read_pc());
}

void ProgramCounter::pc_update(machine::Address val) {
    value.setText(QString("0x") + QString::number(val.get_raw(), 16));
}
