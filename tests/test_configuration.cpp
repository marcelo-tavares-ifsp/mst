#include <QTest>

#include "test.h"
#include "test_configuration.h"

#include "../mst/core/configuration.h"

#include <iostream>

Test_configuration::Test_configuration() : Test()
{

}

void Test_configuration::load_non_existing_config_test()
{
    Configuration config;
    QTemporaryFile system_config;

    if (system_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        config.load(file_name, "");
    }

    system_config.open();
    QString line = system_config.readLine();
    system_config.close();

    QVERIFY2(line == "user:multiseat\n", line.toStdString().c_str());
    QVERIFY2(config.get_seat_count() == 0,
             QString(config.get_seat_count()).toStdString().c_str());
}

void Test_configuration::load_seats_config_test()
{
    Configuration config;
    QTemporaryFile system_config;
    QTemporaryFile seats_config;

    if (system_config.open() && seats_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        QTextStream stream(&seats_config);
        stream << "1 LVDS-1 1280x800 pci-0000:00:1d.2-usb-0:2:1.0-event-kbd platform-i8042-serio-4-event-mouse /devices/pci0000:00/0000:00:1d.7/usb4/4-4"
               << Qt::endl;
        config.load(file_name, seats_config.fileName());
    }

    QVERIFY2(config.get_seat_count() == 1,
             QString(config.get_seat_count()).toStdString().c_str());

    std::shared_ptr<Seat> seat = config.get_seat(1);
    QVERIFY2(seat->get_id() == 1,
             QString(seat->get_id()).toStdString().c_str());
    QVERIFY2(seat->get_monitor().get_interface() == "LVDS-1",
             seat->get_monitor().get_interface().toStdString().c_str());
    QVERIFY2(seat->get_keyboard() == "pci-0000:00:1d.2-usb-0:2:1.0-event-kbd",
             seat->get_keyboard().toStdString().c_str());
    QVERIFY2(seat->get_mouse() == "platform-i8042-serio-4-event-mouse",
             seat->get_mouse().toStdString().c_str());
    QVERIFY2(seat->get_usb() == "/devices/pci0000:00/0000:00:1d.7/usb4/4-4",
             seat->get_usb().toStdString().c_str());
}

void Test_configuration::get_seat_nullptr()
{
    Configuration config;
    QTemporaryFile system_config;
    QTemporaryFile seats_config;

    if (system_config.open() && seats_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        config.load(file_name, seats_config.fileName());
    }

    QVERIFY2(config.get_seat(1) == nullptr,
             "The method returned unexpected value");
}

void Test_configuration::is_valid_zero_seat_test()
{
    Configuration config;
    QTemporaryFile system_config;
    QTemporaryFile seats_config;

    if (system_config.open() && seats_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        config.load(file_name, seats_config.fileName());
    }

    QVERIFY2(! config.is_valid(),
             "Valid config with zero configured seats.");
}

void Test_configuration::is_valid_one_unconfigured_seat_test()
{
    Configuration config;
    QTemporaryFile system_config;
    QTemporaryFile seats_config;

    if (system_config.open() && seats_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        QTextStream stream(&seats_config);
        stream << "1 LVDS-1 1280x800 pci-0000:00:1d.2-usb-0:2:1.0-event-kbd platform-i8042-serio-4-event-mouse"
               << Qt::endl;
        config.load(file_name, seats_config.fileName());
    }

    QVERIFY2(! config.is_valid(),
             "Valid config with one unconfigured seat.");
}


void Test_configuration::is_valid_one_configured_seat_test()
{
    Configuration config;
    QTemporaryFile system_config;
    QTemporaryFile seats_config;

    if (system_config.open() && seats_config.open()) {
        QString file_name = system_config.fileName();
        system_config.remove();
        QTextStream stream(&seats_config);
        stream << "1 LVDS-1 1280x800 pci-0000:00:1d.2-usb-0:2:1.0-event-kbd platform-i8042-serio-4-event-mouse /devices/pci0000:00/0000:00:1d.7/usb4/4-4"
               << Qt::endl;
        config.load(file_name, seats_config.fileName());
    }

    QVERIFY2(config.is_valid(),
             "Valid configuration with one configured seat considered invalid.");
}
