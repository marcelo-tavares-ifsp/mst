#include <QTest>
#include <sstream>

#include "test_seat.h"
#include "../mst/core/types/seat.h"

Test_seat::Test_seat() : QObject()
{

}

void Test_seat::string_from_seat()
{
    QVector<Resolution> resolutions = { Resolution("640x480") };
    Monitor monitor("VGA-1", resolutions);
    Seat seat(0);
    seat.set_keyboard("k1");
    seat.set_mouse("m1");
    seat.set_usb("u1");
    seat.add_monitor(monitor);

    std::stringstream os;
    os << seat;
    QVERIFY2(os.str() == "#<Seat VGA-1 keyboard: k1 mouse: m1 usb: u1>",
             os.str().c_str());
}

void Test_seat::intersects()
{
    QVector<Resolution> resolutions = { Resolution("640x480") };
    Monitor monitor("VGA-1", resolutions);
    Seat seat1(0);
    seat1.set_keyboard("k1");
    seat1.set_mouse("m1");
    seat1.set_usb("u1");
    seat1.add_monitor(monitor);

    Seat seat2(0);
    seat2.set_keyboard("k1");
    seat2.set_mouse("m2");
    seat2.set_usb("u2");
    seat2.add_monitor(monitor);

    QVERIFY2(seat1.intersects(seat2), "seat1 does not intersect seat2");
}

void Test_seat::not_intersects()
{
    QVector<Resolution> resolutions = { Resolution("640x480") };
    Monitor monitor("VGA-1", resolutions);
    Seat seat1(0);
    seat1.set_keyboard("k1");
    seat1.set_mouse("m1");
    seat1.set_usb("u1");
    seat1.add_monitor(monitor);

    Seat seat2(0);
    seat2.set_keyboard("k2");
    seat2.set_mouse("m2");
    seat2.set_usb("u2");
    seat2.add_monitor(monitor);

    QVERIFY2(! seat1.intersects(seat2), "seat1 does not intersect seat2");
}
