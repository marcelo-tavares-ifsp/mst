#ifndef TEST_TEMPLAT_H
#define TEST_TEMPLAT_H

#include <QObject>

class Test_template : public QObject
{
    Q_OBJECT
public:
    explicit Test_template(QObject *parent = nullptr);

signals:

private Q_SLOTS:
    void template_from_string();
    void template_from_cstring();
    void template_manager_get_template();
};

#endif // TEST_TEMPLAT_H
