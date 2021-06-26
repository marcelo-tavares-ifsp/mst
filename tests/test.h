#ifndef TEST_H
#define TEST_H

#include <QtTest>
#include <QObject>

class Test : public QObject
{
    Q_OBJECT
public:
    Test(QObject *parent = nullptr);

signals:

};

#endif // TEST_H
