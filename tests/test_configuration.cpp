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
        std::cout << file_name.toStdString() << std::endl;
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
