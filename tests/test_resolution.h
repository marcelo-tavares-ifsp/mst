#ifndef TEST_RESOLUTION_H
#define TEST_RESOLUTION_H

#include <QObject>

class Test_resolution : public QObject
{
    Q_OBJECT
public:
    Test_resolution();

private Q_SLOTS:
    void parse_string();
    void to_string();

};

#endif // TEST_RESOLUTION_H
