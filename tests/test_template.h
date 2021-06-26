#ifndef TEST_TEMPLAT_H
#define TEST_TEMPLAT_H

#include <QObject>

#include "test.h"

class Test_template : public Test
{
    Q_OBJECT
public:
    explicit Test_template();

signals:

private Q_SLOTS:
    void template_from_string();
    void template_from_cstring();
    void template_manager_get_template();
};

#endif // TEST_TEMPLAT_H
