#include <QString>
#include <QtTest>

#include <iostream>

#include <vector>
#include <string>
#include <memory>

#include "../mst/components/awesome.h"
#include "../mst/core/seat/seat.h"

#include "test_awesome.h"

using namespace std;

Test_awesome::Test_awesome()
{
}

void Test_awesome::make_xephyr_autostart()
{
    QString result = awesome::make_xephyr_autostart();

    QFile expected_output_file("./test_awesome_mst_autostart.lua");
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append(s1.readAll());
    QVERIFY2(result == expected_output, result.toStdString().c_str());
}

void Test_awesome::make_xephyr_rules() {
    QString result = awesome::make_xephyr_rules(1);
    QFile expected_output_file("./test_awesome_xephyr_rules.lua");
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append((s1.readAll()));
    QVERIFY2(result == expected_output, result.toStdString().c_str());
}

/**
 * @brief Test_awesome::make_xephyr_screens -- Test generation of Xephyr screen
 * rules for Awesome.
 */
void Test_awesome::make_xephyr_screens()
{
    vector<shared_ptr<Seat>> seats;
    shared_ptr<Seat> seat = make_shared<Seat>(0);
    QVector<Resolution> resolutions = { Resolution("640x480") };
    Monitor monitor("VGA-1", resolutions);
    seat->add_monitor(monitor);
    seat->set_keyboard("keyboard");
    seat->set_mouse("mouse");
    seat->set_usb("usb");
    seats.push_back(seat);

    QString result = awesome::make_xephyr_screens(seats);

    QFile expected_output_file("./test_awesome_xephyr_screens.lua");
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append(s1.readAll());

    QVERIFY2(result == expected_output, result.toStdString().c_str());
}

