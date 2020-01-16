#ifndef TEST_COMPONENT_H
#define TEST_COMPONENT_H

#include <QObject>

class Test_component : public QObject
{
    Q_OBJECT
public:
    Test_component();

private Q_SLOTS:
    void configuration_test();
};

#endif // TEST_COMPONENT_H
