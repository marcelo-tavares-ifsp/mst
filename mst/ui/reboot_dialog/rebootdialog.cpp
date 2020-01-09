#include "rebootdialog.h"
#include "ui_rebootdialog.h"
#include "platform/platform.h"

Q_LOGGING_CATEGORY(reboot_dialog_category, "mst.reboot_dialog")

RebootDialog::RebootDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::RebootDialog)
{
    ui->setupUi(this);
    this->setWindowTitle("Перезагрузка компьютера");
}

RebootDialog::~RebootDialog()
{
    delete ui;
}

void RebootDialog::on_btnYesReboot_clicked()
{
    Platform::system_reboot();
}

void RebootDialog::on_btnNoReboot_clicked()
{
    this->close();
}
