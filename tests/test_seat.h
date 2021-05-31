#ifndef TEST_SEAT_H
#define TEST_SEAT_H

#include <QObject>

class Test_seat : public QObject
{
    Q_OBJECT
public:
    explicit Test_seat();

private Q_SLOTS:
    void string_from_seat();
    void intersects();
    void not_intersects();
};

#endif // TEST_SEAT_H
