#ifndef REBOOTDIALOG_H
#define REBOOTDIALOG_H

#include <QDialog>
#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(reboot_dialog_category)

namespace Ui {
class RebootDialog;
}

class Reboot_dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Reboot_dialog(QWidget *parent = 0);
    ~Reboot_dialog();

private slots:
    void on_btnYesReboot_clicked();
    void on_btnNoReboot_clicked();

private:
    Ui::RebootDialog *ui;
};

#endif // REBOOTDIALOG_H
