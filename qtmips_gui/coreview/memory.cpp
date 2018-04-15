#include "memory.h"
#include <cmath>

using namespace coreview;

//////////////////////
#define WIDTH 60
#define HEIGHT 80
#define CACHE_HEIGHT 47
#define PENW 1
//////////////////////

Memory::Memory(const machine::Cache *cch) : QGraphicsObject(nullptr), name("Memory", this), type(this), cache_t("Cache", this), cache_hit_t("Hit: 0", this), cache_miss_t("Miss: 0", this) {
    cache = false;

    QFont font;
    font.setPointSize(7);
    name.setFont(font);
    type.setFont(font);
    cache_t.setFont(font);
    cache_hit_t.setFont(font);
    cache_miss_t.setFont(font);

    name.setPos(WIDTH/2 - name.boundingRect().width()/2, HEIGHT - (HEIGHT - CACHE_HEIGHT)/2);
    const QRectF &cache_t_b = cache_t.boundingRect();
    cache_t.setPos(WIDTH/2 - cache_t_b.width()/2, 1);
    const QRectF &cache_hit_b = cache_hit_t.boundingRect();
    cache_hit_t.setPos(WIDTH/2 - cache_hit_b.width()/2, cache_t_b.height() + 2);
    cache_miss_t.setPos(WIDTH/2 - cache_miss_t.boundingRect().width()/2, cache_t_b.height() + cache_hit_b.height() + 3);

    connect(cch, SIGNAL(hit_update(uint)), this, SLOT(cache_hit_update(uint)));
    connect(cch, SIGNAL(miss_update(uint)), this, SLOT(cache_miss_update(uint)));

    setPos(x(), y()); // set connector's position
}

QRectF Memory::boundingRect() const {
    return QRectF(-PENW / 2, -PENW / 2, WIDTH + PENW, HEIGHT + PENW);
}

void Memory::paint(QPainter *painter, const QStyleOptionGraphicsItem *option __attribute__((unused)), QWidget *widget __attribute__((unused))) {
    painter->drawRect(0, 0, WIDTH, HEIGHT);
    if (cache)
        painter->drawLine(0, CACHE_HEIGHT, WIDTH, CACHE_HEIGHT);
}

void Memory::cache_hit_update(unsigned val) {
    cache_hit_t.setText("Hit: " + QString::number(val));
}

void Memory::cache_miss_update(unsigned val) {
    cache_miss_t.setText("Miss: " + QString::number(val));
}

void Memory::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event) {
    QGraphicsObject::mouseDoubleClickEvent(event);

    if (cache && event->pos().y() < HEIGHT/2)
        emit open_cache();
    else
        emit open_mem();
}

void Memory::set_type(const QString &text) {
    type.setText(text);
    const QRectF &box = type.boundingRect();
    type.setPos(WIDTH/2 - box.width()/2, HEIGHT - (HEIGHT - CACHE_HEIGHT)/2 - box.height());
}

ProgramMemory::ProgramMemory(machine::QtMipsMachine *machine) : Memory(machine->cache_program()) {
    cache = machine->config().cache_program().enabled();
    set_type("Program");

    con_address = new Connector(Connector::AX_X);
    con_inst = new Connector(Connector::AX_X);
}

ProgramMemory::~ProgramMemory() {
    delete con_address;
    delete con_inst;
}

void ProgramMemory::setPos(qreal x, qreal y) {
    QGraphicsObject::setPos(x, y);

    con_address->setPos(x, y + 20);
    con_inst->setPos(x + WIDTH, y + 20);
}

const Connector *ProgramMemory::connector_address() const {
    return con_address;
}

const Connector *ProgramMemory::connector_instruction() const {
    return con_inst;
}

DataMemory::DataMemory(machine::QtMipsMachine *machine) : Memory(machine->cache_data()) {
    cache = machine->config().cache_data().enabled();
    set_type("Data");

    con_address = new Connector(Connector::AX_X);
    con_data_out = new Connector(Connector::AX_X);
    con_data_in	= new Connector(Connector::AX_X);
    con_req_write = new Connector(Connector::AX_Y);
    con_req_read = new Connector(Connector::AX_Y);
}

DataMemory::~DataMemory() {
    delete con_address;
    delete con_data_out;
    delete con_data_in;
    delete con_req_write;
    delete con_req_read;
}

void DataMemory::setPos(qreal x, qreal y) {
    QGraphicsObject::setPos(x, y);

    con_address->setPos(x, y + 20);
    con_data_out->setPos(x + WIDTH, y + 20);
    if (cache)
        con_data_in->setPos(x, y + 40);
    else
        con_data_in->setPos(x, y + 60);
    con_req_write->setPos(x + 40, y);
    con_req_read->setPos(x + 50, y);
}

const Connector *DataMemory::connector_address() const {
    return con_address;
}

const Connector *DataMemory::connector_data_out() const {
    return con_data_out;
}

const Connector *DataMemory::connector_data_in() const {
    return con_data_in;
}

const Connector *DataMemory::connector_req_write() const {
    return con_req_write;
}

const Connector *DataMemory::connector_req_read() const {
    return con_req_read;
}
