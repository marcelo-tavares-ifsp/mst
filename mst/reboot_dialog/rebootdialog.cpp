#include "rebootdialog.h"
#include "ui_rebootdialog.h"
#include "command_manager/commandmanager.h"

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
    CommandManager::reboot_autoboot();
}

void RebootDialog::on_btnNoReboot_clicked()
{
    this->close();
}
