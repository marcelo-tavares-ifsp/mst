#include <QString>
#include <QtTest>

#include <iostream>

#include <vector>
#include <string>

#include "../mst/config_manager/awesome.h"

#include "test_awesome.h"

using namespace std;

Test_awesome::Test_awesome()
{
}

void Test_awesome::make_xephyr_rules() {
    string result = Awesome::make_xephyr_rules(1);
    QFile expected_output_file("./test_awesome_xephyr_rules.lua");
    QWARN(QDir::currentPath().toStdString().c_str());
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append((s1.readAll()));
    QVERIFY2(result == expected_output.toStdString(), "Results differ.");
}

/**
 * @brief Test_awesome::make_xephyr_screens -- Test generation of Xephyr screen
 * rules for Awesome.
 */
void Test_awesome::make_xephyr_screens()
{
    vector<Seat> seats;
    Seat seat = {
        .interface = "interface",
        .mouse     = "mouse",
        .keyboard  = "keyboard",
        .usb       = "usb",
        .resolution = { .width = 640, .height = 480 }
    };
    seats.push_back(seat);
    string result = Awesome::make_xephyr_screens(seats);

    QFile expected_output_file("./test_awesome_xephyr_screens.lua");
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append(s1.readAll());

    QVERIFY2(result == expected_output.toStdString(), "Results differ.");
}

