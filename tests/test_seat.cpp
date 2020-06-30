#include <QTest>
#include <sstream>

#include "test_seat.h"
#include "../mst/common/seat/seat.h"

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
