#include <QtTest>

#include "test_platform.h"
#include "../mst/core/platform.h"

Test_platform::Test_platform()
{

}

void Test_platform::parse_devices_test()
{
    QVector<QString> devices = {
        "pci-0000:00:1d.1-usb-0:2:1.0-event",
        "pci-0000:00:1d.1-usb-0:2:1.0-event-mouse",
        "pci-0000:00:1d.1-usb-0:2:1.0-mouse",
        "pci-0000:00:1d.2-usb-0:2:1.0-event",
        "pci-0000:00:1d.2-usb-0:2:1.0-event-kbd",
        "pci-0000:00:1d.2-usb-0:2:1.1-event",
        "platform-i8042-serio-0-event",
        "platform-i8042-serio-0-event-kbd",
        "platform-i8042-serio-4-event-mouse",
        "platform-i8042-serio-4-mouse",
        "platform-pcspkr-event-spkr"
    };

    QVector<QString> mice;
    QVector<QString> keyboards;

    platform::parse_devices(devices, mice, keyboards);

    QVERIFY( mice[0]      == "pci-0000:00:1d.1-usb-0:2:1.0-event-mouse" );
    QVERIFY( keyboards[0] == "pci-0000:00:1d.2-usb-0:2:1.0-event-kbd" );
}
