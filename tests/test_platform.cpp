#include <QtTest>
#include <sys/stat.h>
#include <iostream>

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

void Test_platform::fs_rm_test()
{
    QTemporaryFile tmp_file;
    tmp_file.open();
    Platform::fs_rm(tmp_file.fileName());
    QVERIFY(tmp_file.exists() == false);
}

void Test_platform::fs_rm_fail_test()
{
    QTemporaryFile tmp_file;
    tmp_file.open();
    tmp_file.remove();
    QVERIFY_EXCEPTION_THROWN(Platform::fs_rm(tmp_file.fileName()),
                             Platform_exception);
}

void Test_platform::fs_mkdir_test()
{
    QTemporaryDir tmp_dir;
    QString test_path = tmp_dir.path() + "/test";
    Platform::fs_mkdir(test_path);
    QDir test_dir(test_path);

    QVERIFY(test_dir.exists() == true);

    test_dir.removeRecursively();
    tmp_dir.remove();
}

void Test_platform::fs_mkdir_fail_test()
{
    QString test_path = "";
    QVERIFY_EXCEPTION_THROWN(Platform::fs_mkdir(test_path),
                             Platform_exception);
}
