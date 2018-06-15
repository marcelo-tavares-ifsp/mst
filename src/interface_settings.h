#ifndef INTERFACE_SETTINGS_H
#define INTERFACE_SETTINGS_H

#include <QWidget>

namespace Ui {
class interface_settings;
}

class interface_settings : public QWidget
{
    Q_OBJECT

public:
    explicit interface_settings(QWidget *parent = 0);
    ~interface_settings();

private slots:
    void on_btn_next_2_clicked();

    void on_pushButton_clicked();

private:
    Ui::interface_settings *ui;
};

#endif // INTERFACE_SETTINGS_H
