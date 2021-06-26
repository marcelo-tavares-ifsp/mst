#ifndef TEST_MSTD_H
#define TEST_MSTD_H

#include <QObject>

#include "test.h"

class Test_mstd : public Test
{
    Q_OBJECT
public:
    explicit Test_mstd();
    bool run_guile_test(QString file_name);

signals:

private Q_SLOTS:
    void config_test();
    void dm_test();
    void system_test();
};

#endif // TEST_MSTD_H
