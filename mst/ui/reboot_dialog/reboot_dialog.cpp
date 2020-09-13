#include "reboot_dialog.h"
#include "ui_rebootdialog.h"
#include "core/platform.h"

Q_LOGGING_CATEGORY(reboot_dialog_category, "mst.reboot_dialog")

Reboot_dialog::Reboot_dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::RebootDialog)
{
    ui->setupUi(this);
    this->setWindowTitle(tr("Reboot"));
}

Reboot_dialog::~Reboot_dialog()
{
    delete ui;
}

void Reboot_dialog::on_btnYesReboot_clicked()
{
    Platform::system_reboot();
}

void Reboot_dialog::on_btnNoReboot_clicked()
{
    this->close();
}
