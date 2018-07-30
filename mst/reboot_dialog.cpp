#include "reboot_dialog.h"
#include "ui_reboot_dialog.h"
#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>

Reboot_Dialog::Reboot_Dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Reboot_Dialog)
{
    ui->setupUi(this);
}

Reboot_Dialog::~Reboot_Dialog()
{
    delete ui;
}

void Reboot_Dialog::on_btn_now_reboot_clicked()
{
    sync();
    setuid(0);
    reboot(RB_AUTOBOOT);
}

void Reboot_Dialog::on_btn_late_reboot_clicked()
{
    this->close();
}
