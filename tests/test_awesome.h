#ifndef TEST_AWESOME_H
#define TEST_AWESOME_H

#include <QtTest>

class Test_awesome : public QObject
{
    Q_OBJECT

public:
    Test_awesome();

private Q_SLOTS:
    void make_xephyr_autostart();
    void make_xephyr_rules();
};

#endif // TEST_AWESOME_H
