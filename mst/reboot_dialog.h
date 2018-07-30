#ifndef REBOOT_DIALOG_H
#define REBOOT_DIALOG_H

#include <QDialog>

namespace Ui {
class Reboot_Dialog;
}

class Reboot_Dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Reboot_Dialog(QWidget *parent = 0);
    ~Reboot_Dialog();

private slots:
    void on_btn_now_reboot_clicked();

    void on_btn_late_reboot_clicked();

private:
    Ui::Reboot_Dialog *ui;
};

#endif // REBOOT_DIALOG_H
